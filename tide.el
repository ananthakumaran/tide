;;; tide.el --- Typescript Interactive Development Environment -*- lexical-binding: t -*-

;; Copyright (C) 2015 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; URL: http://github.com/ananthakumaran/tide
;; Version: 2.1.4
;; Keywords: typescript
;; Package-Requires: ((dash "2.10.0") (flycheck "27") (typescript-mode "0.1") (cl-lib "0.5"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'typescript-mode)
(require 'etags)
(require 'json)
(require 'cl-lib)
(require 'eldoc)
(require 'dash)
(require 'flycheck)
(require 'imenu)

;; Silence compiler warnings

(defvar js2-basic-offset)
(defvar js-indent-level)
(defvar js3-indent-level)
(defvar web-mode-code-indent-offset)
(defvar sgml-basic-offset)
(defvar company-backends)

(declare-function company-grab-symbol-cons "company.el" (idle-begin-after-re &optional max-len))
(declare-function company-begin-backend "company.el" (backend &optional callback))
(declare-function company-in-string-or-comment "company.el" nil)

(defgroup tide nil
  "TypeScript Interactive Development Environment."
  :prefix "tide-"
  :group 'tools)

(defcustom tide-sync-request-timeout 2
  "The number of seconds to wait for a sync response."
  :type 'integer
  :group 'tide)

(defcustom tide-tsserver-process-environment '()
  "List of extra environment variables to use when starting tsserver."
  :type '(repeat string)
  :group 'tide)

(defcustom tide-tsserver-executable nil
  "Name of tsserver executable to run instead of the bundled tsserver.

This may either be an absolute path or a relative path. Relative
paths are resolved against the project root directory.

Note that this option only works with TypeScript version 2.0 and
above."
  :type '(choice (const nil) string)
  :group 'tide)

(defcustom tide-node-executable "node"
  "Name or path of the node executable binary file."
  :type '(choice (const nil) string)
  :group 'tide)

(defvar tide-format-options '()
  "Format options plist.")

(defvar tide-completion-ignore-case nil
  "CASE will be ignored in completion if set to non-nil.")

(defvar tide-completion-detailed nil
  "Completion dropdown will contain detailed method information if set to non-nil.")

(defface tide-file
  '((t (:inherit dired-header)))
  "Face for file names in references output."
  :group 'tide)

(defface tide-line-number
  '((t (:inherit compilation-line-number)))
  "Face for line numbers in references output."
  :group 'tide)

(defface tide-match
  '((t (:inherit match)))
  "Face for matched symbol in references output."
  :group 'tide)

(defface tide-imenu-type-face
  '((t (:inherit font-lock-type-face)))
  "Face for type in imenu list."
  :group 'tide)

(defcustom tide-jump-to-definition-reuse-window t
  "Reuse existing window when jumping to definition."
  :type 'boolean
  :group 'tide)

(defmacro tide-def-permanent-buffer-local (name &optional init-value)
  "Declare NAME as buffer local variable."
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defvar tide-supported-modes '(typescript-mode web-mode js-mode js2-mode js2-jsx-mode js3-mode))

(defvar tide-server-buffer-name "*tide-server*")
(defvar tide-request-counter 0)

(tide-def-permanent-buffer-local tide-project-root nil)
(tide-def-permanent-buffer-local tide-buffer-dirty nil)
(tide-def-permanent-buffer-local tide-buffer-tmp-file nil)

(defvar tide-servers (make-hash-table :test 'equal))
(defvar tide-response-callbacks (make-hash-table :test 'equal))

(defvar tide-source-root-directory (file-name-directory load-file-name))
(defvar tide-tsserver-directory (expand-file-name "tsserver" tide-source-root-directory))

(defun tide-project-root ()
  "Project root folder determined based on the presence of tsconfig.json."
  (or
   tide-project-root
   (let ((root (or (locate-dominating-file default-directory "tsconfig.json")
                   (locate-dominating-file default-directory "jsconfig.json"))))
     (unless root
       (message (tide-join (list "Couldn't locate project root folder with a tsconfig.json or jsconfig.json file. Using '" default-directory "' as project root.")))
       (setq root default-directory))
     (let ((full-path (expand-file-name root)))
       (setq tide-project-root full-path)
       full-path))))

(defun tide-project-name ()
  (file-name-nondirectory (directory-file-name (tide-project-root))))

;;; Compatibility

(defvar tide-tsserver-unsupported-commands (make-hash-table :test 'equal))

(defun tide-mark-as-unsupported (command)
  (puthash
   (tide-project-name)
   (cl-pushnew
    command
    (gethash (tide-project-name) tide-tsserver-unsupported-commands '()))
   tide-tsserver-unsupported-commands))

(defun tide-unsupported-p (command)
  (member command (gethash (tide-project-name) tide-tsserver-unsupported-commands '())))

(defmacro tide-fallback-if-not-supported (new-command new old)
  `(if (tide-unsupported-p ,new-command)
       (let ((response (,old)))
         (when (tide-response-success-p response)
           response))
     (let ((response (,new)))
       (if (tide-command-unknown-p response)
           (progn
             (tide-mark-as-unsupported ,new-command)
             (let ((response (,old)))
               (when (tide-response-success-p response)
                 response)))
         (when (tide-response-success-p response)
           response)))))

;;; Helpers

(defun tide-plist-get (list &rest args)
  (cl-reduce
   (lambda (object key)
     (when object
       (plist-get object key)))
   args
   :initial-value list))

(defun tide-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists. Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(defun tide-response-success-p (response)
  (and response (equal (plist-get response :success) t)))

(defun tide-command-unknown-p (response)
  (and response (string-equal (plist-get response :command) "unknown")))

(defmacro tide-on-response-success (response &rest body)
  (declare (indent 1))
  `(if (tide-response-success-p ,response)
       ,@body
     (-when-let (msg (plist-get response :message))
       (message "%s" msg))
     nil))

(defun tide-join (list)
  (mapconcat 'identity list ""))

(defun tide-each-buffer (project-name fn)
  "Callback FN for each buffer within PROJECT-NAME with tide-mode enabled."
  (-each (buffer-list)
    (lambda (buffer)
      (with-current-buffer buffer
        (when (and (bound-and-true-p tide-mode)
                   (equal (tide-project-name) project-name))
          (funcall fn))))))

(defun tide-line-number-at-pos (&optional pos)
  (let ((p (or pos (point))))
    (if (= (point-min) 1)
        (line-number-at-pos p)
      (save-excursion
        (save-restriction
          (widen)
          (line-number-at-pos p))))))

(defun tide-current-offset ()
  "Number of characters present from the begining of line to cursor in current line.

offset is one based."
  (1+ (- (point) (line-beginning-position))))

(defun tide-offset (pos)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (tide-current-offset))))

(defun tide-column (line offset)
  "Return column number corresponds to the LINE and OFFSET.

LINE is one based, OFFSET is one based and column is zero based"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (beginning-of-line)
      (while (> offset 1)
        (forward-char)
        (cl-decf offset))
      (1+ (current-column)))))

(defun tide-span-to-position (span)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- (plist-get span :line)))
      (beginning-of-line)
      (forward-char (1- (plist-get span :offset)))
      (point))))

(defun tide-doc-buffer (string)
  (with-current-buffer (get-buffer-create "*tide-documentation*")
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when string
        (save-excursion
          (insert string))))
    (local-set-key (kbd "q") #'quit-window)
    (current-buffer)))

;;; Events

(defvar tide-event-listeners (make-hash-table :test 'equal))

(defun tide-set-event-listener (listener)
  (puthash (tide-project-name) (cons (current-buffer) listener) tide-event-listeners))

(defun tide-clear-event-listener ()
  (remhash (tide-project-name) tide-event-listeners))

;;; Server

(defun tide-current-server ()
  (gethash (tide-project-name) tide-servers))

(defun tide-next-request-id ()
  (number-to-string (cl-incf tide-request-counter)))

(defun tide-dispatch-response (response)
  (let* ((request-id (plist-get response :request_seq))
         (callback (gethash request-id tide-response-callbacks)))
    (when callback
      (with-current-buffer (car callback)
        (apply (cdr callback) (list response)))
      (remhash request-id tide-response-callbacks))))

(defun tide-dispatch-event (event)
  (-when-let (listener (gethash (tide-project-name) tide-event-listeners))
    (with-current-buffer (car listener)
      (apply (cdr listener) (list event)))))

(defun tide-dispatch (response)
  (cl-case (intern (plist-get response :type))
    ('response (tide-dispatch-response response))
    ('event (tide-dispatch-event response))))

(defun tide-send-command (name args &optional callback)
  (when (not (tide-current-server))
    (error "Server does not exist. Run M-x tide-restart-server to start it again"))

  (tide-sync-buffer-contents)

  (let* ((request-id (tide-next-request-id))
         (command `(:command ,name :seq ,request-id :arguments ,args))
         (encoded-command (json-encode command))
         (payload (concat encoded-command "\n")))
    (process-send-string (tide-current-server) payload)
    (when callback
      (puthash request-id (cons (current-buffer) callback) tide-response-callbacks)
      (accept-process-output nil 0.01))))

(defun tide-send-command-sync (name args)
  (let* ((start-time (current-time))
         (response nil))
    (tide-send-command name args (lambda (resp) (setq response resp)))
    (while (not response)
      (accept-process-output nil 0.01)
      (when (> (cadr (time-subtract (current-time) start-time))
               tide-sync-request-timeout)
        (error "Sync request timed out %s" name)))
    response))

(defun tide-net-filter (process data)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert data))
  (tide-decode-response process))

(defun tide-net-sentinel (process message)
  (let ((project-name (process-get process 'project-name)))
    (message "(%s) tsserver exits: %s." project-name (string-trim message))
    (ignore-errors
      (kill-buffer (process-buffer process)))
    (tide-cleanup-project project-name)))

(defun tide-start-server ()
  (when (tide-current-server)
    (error "Server already exist"))

  (message "(%s) Starting tsserver..." (tide-project-name))
  (let* ((default-directory (tide-project-root))
         (process-environment (append tide-tsserver-process-environment process-environment))
         (buf (generate-new-buffer tide-server-buffer-name))
         (tsserverjs (or (and tide-tsserver-executable
                              (expand-file-name tide-tsserver-executable))
                         (expand-file-name "tsserver.js" tide-tsserver-directory)))
         (process
          (start-file-process "tsserver" buf tide-node-executable tsserverjs)))
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (set-process-filter process #'tide-net-filter)
    (set-process-sentinel process #'tide-net-sentinel)
    (set-process-query-on-exit-flag process nil)
    (process-put process 'project-name (tide-project-name))
    (puthash (tide-project-name) process tide-servers)
    (message "(%s) tsserver server started successfully." (tide-project-name))))

(defun tide-cleanup-buffer-callbacks ()
  (let ((error-response `(:success ,nil)))
    (maphash
     (lambda (id callback)
       (when (equal (current-buffer) (car callback))
         (funcall (cdr callback) error-response)
         (remhash id tide-response-callbacks)))
     tide-response-callbacks)))

(defun tide-cleanup-project (project-name)
  (tide-each-buffer project-name
                    (lambda ()
                      (tide-cleanup-buffer-callbacks)))
  (remhash project-name tide-servers)
  (remhash project-name tide-tsserver-unsupported-commands)
  (remhash project-name tide-project-configs))

(defun tide-start-server-if-required ()
  (when (not (tide-current-server))
    (tide-start-server)))

(defun tide-decode-response-legth ()
  (goto-char (point-min))
  (when (re-search-forward "Content-Length: \\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun tide-enough-response-p (length)
  (save-excursion
    (when (search-forward "{" nil t)
      (backward-char 1)
      (>= (- (position-bytes (point-max)) (position-bytes (point))) (1- length)))))

(defun tide-decode-response (process)
  (with-current-buffer (process-buffer process)
    (let ((length (tide-decode-response-legth))
          (json-object-type 'plist)
          (json-array-type 'list))
      (when (and length (tide-enough-response-p length))
        (tide-dispatch
         (prog2
             (progn
               (search-forward "{")
               (backward-char 1))
             (json-read-object)
           (delete-region (point-min) (point))))
        (when (>= (buffer-size) 16)
          (tide-decode-response process))))))

;;; Initialization

(defun tide-file-format-options ()
  (tide-combine-plists
   `(:tabSize ,tab-width :indentSize ,(tide-current-indentsize))
   tide-format-options))

(defun tide-current-indentsize ()
  (pcase major-mode
    (`typescript-mode typescript-indent-level)
    (`js2-mode js2-basic-offset)
    (`js-mode js-indent-level)
    (`js3-mode js3-indent-level)
    (`web-mode web-mode-code-indent-offset)
    (`js2-jsx-mode sgml-basic-offset)
    (_ standard-indent)))

(defun tide-command:configure ()
  (tide-send-command "configure" `(:hostInfo ,(emacs-version) :file ,buffer-file-name :formatOptions ,(tide-file-format-options))))

(defun tide-command:projectInfo (cb &optional need-file-name-list)
  (tide-send-command "projectInfo" `(:file ,buffer-file-name :needFileNameList ,need-file-name-list) cb))

(defun tide-command:openfile ()
  (tide-send-command "open"
                     (append `(:file ,buffer-file-name)
                             (let ((extension (upcase (file-name-extension buffer-file-name))))
                               (if (member extension '("TS" "JS" "TSX" "JSX"))
                                   `(:scriptKindName ,extension)
                                 nil)))))

(defun tide-command:closefile ()
  (tide-send-command "close" `(:file ,buffer-file-name)))

;;; Jump to definition

(defun tide-command:definition (cb)
  (tide-send-command
   "definition"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))
   cb))

(defun tide-command:type-definition (cb)
  (tide-send-command
   "typeDefinition"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))
   cb))

(defun tide-jump-to-definition (&optional arg)
  "Jump to the definition of the symbol at point.

With a prefix arg, Jump to the type definition."
  (interactive "P")
  (let ((cb (lambda (response)
              (tide-on-response-success response
                (let ((filespan (car (plist-get response :body))))
                  (tide-jump-to-filespan filespan tide-jump-to-definition-reuse-window))))))
    (if arg
        (tide-command:type-definition cb)
      (tide-command:definition cb))))

(defun tide-move-to-location (location)
  (let* ((line (plist-get location :line))
         (offset (plist-get location :offset)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))
    (forward-char (1- offset))))

(defun tide-location-to-point (location)
  (save-excursion
    (tide-move-to-location location)
    (point)))

(defun tide-jump-to-filespan (filespan &optional reuse-window no-marker)
  (let ((file (plist-get filespan :file)))
    (unless no-marker
      (ring-insert find-tag-marker-ring (point-marker)))
    (if reuse-window
        (pop-to-buffer (find-file-noselect file) '((display-buffer-reuse-window display-buffer-same-window)))
      (pop-to-buffer (find-file-noselect file)))
    (tide-move-to-location (plist-get filespan :start))))

(defalias 'tide-jump-back 'pop-tag-mark)

;;; Eldoc

(defun tide-annotate-display-part (display-part &optional highlight)
  (let ((text (plist-get display-part :text))
        (face (pcase (plist-get display-part :kind)
                ("aliasName" 'font-lock-type-face)
                ("className" 'font-lock-type-face)
                ("enumName" 'font-lock-type-face)
                ("fieldName" nil)
                ("interfaceName" 'font-lock-type-face)
                ("keyword" 'font-lock-keyword-face)
                ("lineBreak" nil)
                ("numericLiteral" nil)
                ("stringLiteral" 'font-lock-string-face)
                ("localName" 'font-lock-variable-name-face)
                ("methodName" nil)
                ("moduleName" nil)
                ("operator" nil)
                ("parameterName" (and highlight 'eldoc-highlight-function-argument))
                ("propertyName" nil)
                ("punctuation" nil)
                ("space" nil)
                ("text" nil)
                ("typeParameterName" 'font-lock-variable-name-face)
                ("enumMemberName" 'font-lock-constant-face)
                ("functionName" 'font-lock-function-name-face)
                ("regularExpressionLiteral" 'font-lock-string-face))))
    (if face
        (propertize text 'face face)
      text)))

(defun tide-annotate-display-parts (display-parts &optional highlight)
  (tide-join (-map (lambda (part) (tide-annotate-display-part part highlight)) display-parts)))

(defun tide-annotate-signature-parameter (parameter highlight)
  (tide-join
   (-map
    (lambda (part) (tide-annotate-display-part part highlight))
    (plist-get parameter :displayParts))))

(defun tide-annotate-signature (signature selected-arg-index)
  (let ((separator (tide-join (-map #'tide-annotate-display-part (plist-get signature :separatorDisplayParts)))))
    (tide-join
     (-concat
      (-map #'tide-annotate-display-part (plist-get signature :prefixDisplayParts))
      (list
       (mapconcat
        #'identity
        (-map-indexed
         (lambda (i parameter)
           (tide-annotate-signature-parameter parameter (eq i selected-arg-index)))
         (plist-get signature :parameters))
        separator))
      (-map #'tide-annotate-display-part (plist-get signature :suffixDisplayParts))))))

(defun tide-annotate-signatures (body)
  (let ((selected-index (plist-get body :selectedItemIndex))
        (selected-arg-index (plist-get body :argumentIndex)))
    (tide-annotate-signature
     (nth selected-index (plist-get body :items))
     selected-arg-index)))

(defun tide-command:signatureHelp ()
  (let* ((response
          (tide-send-command-sync
           "signatureHelp"
           `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset)))))
    (when (tide-response-success-p response)
      (tide-annotate-signatures (plist-get response :body)))))

(defun tide-method-call-p ()
  (or (looking-at "[(,]") (and (not (looking-at "\\sw")) (looking-back "[(,]\n?\\s-*"))))

(defun tide-quickinfo-text (response)
  (or (tide-plist-get response :body :displayString) ;; old
      (tide-annotate-display-parts
       (tide-plist-get response :body :displayParts))))

(defun tide-quickinfo-documentation (response)
  (let ((documentation (tide-plist-get response :body :documentation)))
    (if (stringp documentation) ;; old
        documentation
      (tide-annotate-display-parts documentation))))

(defun tide-command:quickinfo-old ()
  (tide-send-command-sync "quickinfo" `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))))

(defun tide-command:quickinfo-full ()
  (tide-send-command-sync "quickinfo-full" `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))))

(defun tide-command:quickinfo ()
  (tide-fallback-if-not-supported "quickinfo-full" tide-command:quickinfo-full tide-command:quickinfo-old))


(defun tide-eldoc-function ()
  (when (not (member last-command '(next-error previous-error)))
    (if (tide-method-call-p)
        (tide-command:signatureHelp)
      (when (looking-at "\\s_\\|\\sw")
        (-when-let (quickinfo (tide-command:quickinfo))
          (tide-quickinfo-text quickinfo))))))


(defun tide-documentation-at-point ()
  "Show documentation of the symbol at point."
  (interactive)
  (let ((documentation
         (-when-let* ((quickinfo (tide-command:quickinfo))
                      (display-string (tide-quickinfo-text quickinfo))
                      (documentation (tide-quickinfo-documentation quickinfo)))
           (when (not (equal documentation ""))
             (tide-join (list display-string "\n\n" documentation))))))
    (if documentation
        (display-buffer (tide-doc-buffer documentation) t)
      (message "No documentation available"))))

;;; Buffer Sync

(defun tide-remove-tmp-file ()
  (when tide-buffer-tmp-file
    (delete-file tide-buffer-tmp-file)
    (setq tide-buffer-tmp-file nil)))

(defun tide-command:reloadfile ()
  (tide-send-command "reload" `(:file ,buffer-file-name :tmpfile ,buffer-file-name)))

(defun tide-handle-change (_beg _end _len)
  (setq tide-buffer-dirty t))

(defun tide-sync-buffer-contents ()
  (when tide-buffer-dirty
    (setq tide-buffer-dirty nil)
    (when (not tide-buffer-tmp-file)
      (setq tide-buffer-tmp-file (make-temp-file "tide")))
    (write-region (point-min) (point-max) tide-buffer-tmp-file nil 'no-message)
    (tide-send-command "reload" `(:file ,buffer-file-name :tmpfile ,tide-buffer-tmp-file))))

;;; Auto completion

(defun tide-completion-annotation (name)
  (if tide-completion-detailed
      ;; Get everything before the first newline, if any, because company-mode
      ;; wants single-line annotations.
      (car (split-string (tide-completion-meta name) "\n"))
    (pcase (plist-get (get-text-property 0 'completion name) :kind)
      ("keyword" " k")
      ("module" " M")
      ("class" " C")
      ("interface" " I")
      ("type" " T")
      ("enum" " E")
      ("var" " v")
      ("local var" " v")
      ("function" " ƒ")
      ("local function" " ƒ")
      ("method" " m")
      ("getter" " m")
      ("setter" " m")
      ("property" " p")
      ("constructor" " c")
      ("call" " m")
      ("index" " i")
      ("construct" " m")
      ("parameter" " p")
      ("type parameter" " T")
      ("primitive type" " T")
      ("label" " l")
      ("alias" " A")
      ("const" " c")
      ("let" " l")
      (t nil))))

(defun tide-completion-prefix ()
  (company-grab-symbol-cons "\\." 1))

(defun tide-member-completion-p (prefix)
  (save-excursion
    (backward-char (length prefix))
    (and (> (point) (point-min))
         (equal (string (char-before (point))) "."))))

(defun tide-annotate-completions (completions prefix file-location)
  (-map
   (lambda (completion)
     (let ((name (plist-get completion :name)))
       (put-text-property 0 1 'file-location file-location name)
       (put-text-property 0 1 'completion completion name)
       name))
   (-filter
    (lambda (completion)
      (string-prefix-p prefix (plist-get completion :name)))
    completions)))

(defun tide-command:completions (prefix cb)
  (let* ((file-location
          `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(- (tide-current-offset) (length prefix)))))
    (when (not (tide-member-completion-p prefix))
      (setq file-location (plist-put file-location :prefix prefix)))
    (tide-send-command
     "completions"
     file-location
     (lambda (response)
       (funcall
        cb
        (when (tide-response-success-p response)
          (tide-annotate-completions (plist-get response :body) prefix file-location)))))))

(defun tide-format-detail-type (detail)
  (tide-join
   (-map (lambda (part) (tide-annotate-display-part part)) (plist-get detail :displayParts))))

(defun tide-command:completion-entry-details (name)
  (let ((arguments (-concat (get-text-property 0 'file-location name)
                            `(:entryNames (,name)))))
    (-when-let (response (tide-send-command-sync "completionEntryDetails" arguments))
      (when (tide-response-success-p response)
        response))))

(defun tide-completion-entry-details (name)
  (-if-let (detail-response (get-text-property 0 'completion-detail name))
      detail-response
    (let ((detail-response (tide-command:completion-entry-details name)))
      (put-text-property 0 1 'completion-detail detail-response name)
      detail-response)))

(defun tide-completion-meta (name)
  (-when-let* ((response (tide-completion-entry-details name))
               (detail (car (plist-get response :body))))
    (tide-format-detail-type detail)))

(defun tide-completion-doc-buffer (name)
  (-when-let* ((response (tide-completion-entry-details name))
               (detail (car (plist-get response :body)))
               (documentation (plist-get detail :documentation)))
    (tide-doc-buffer
     (tide-join
      (list (tide-format-detail-type detail)
            "\n\n"
            (tide-join (-map #'tide-annotate-display-part documentation)))))))

;;;###autoload
(defun company-tide (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tide))
    (prefix (and
             (bound-and-true-p tide-mode)
             (-any-p #'derived-mode-p tide-supported-modes)
             (tide-current-server)
             (not (company-in-string-or-comment))
             (or (tide-completion-prefix) 'stop)))
    (candidates (cons :async
                      (lambda (cb)
                        (tide-command:completions arg cb))))
    (sorted t)
    (ignore-case tide-completion-ignore-case)
    (meta (tide-completion-meta arg))
    (annotation (tide-completion-annotation arg))
    (doc-buffer (tide-completion-doc-buffer arg))))

(eval-after-load 'company
  '(progn
     (cl-pushnew 'company-tide company-backends)))

;;; References

(defun tide-next-reference-function (n &optional reset)
  "Override for `next-error-function' for use in tide-reference-mode buffers."
  (interactive "p")

  (-when-let (buffer (get-buffer "*tide-references*"))
    (with-current-buffer buffer
      (when reset
        (goto-char (point-min)))
      (if (> n 0)
          (tide-find-next-reference (point) n)
        (tide-find-previous-reference (point) (- n)))
      (tide-goto-reference))))

(defun tide-find-next-reference (pos arg)
  "Move to next reference."
  (interactive "d\np")
  (setq arg (* 2 arg))
  (unless (get-text-property pos 'tide-reference)
    (setq arg (1- arg)))
  (dotimes (_i arg)
    (setq pos (next-single-property-change pos 'tide-reference))
    (unless pos
      (error "Moved past last reference")))
  (goto-char pos))

(defun tide-find-previous-reference (pos arg)
  "Move back to previous reference."
  (interactive "d\np")
  (dotimes (_i (* 2 arg))
    (setq pos (previous-single-property-change pos 'tide-reference))
    (unless pos
      (error "Moved back before first reference")))
  (goto-char pos))

(defun tide-goto-reference ()
  "Jump to reference location in the file."
  (interactive)
  (-when-let (reference (get-text-property (point) 'tide-reference))
    (tide-jump-to-filespan reference nil t)))

(defvar tide-references-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tide-find-next-reference)
    (define-key map (kbd "p") #'tide-find-previous-reference)
    (define-key map (kbd "C-m") #'tide-goto-reference)
    (define-key map [mouse-1] #'tide-goto-reference)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode tide-references-mode nil "tide-references"
  "Major mode for tide references list.

\\{tide-references-mode-map}"
  (use-local-map tide-references-mode-map)
  (setq buffer-read-only t)
  (setq next-error-function #'tide-next-reference-function))

(defun tide-command:references ()
  (tide-send-command-sync
   "references"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))))

(defun tide-annotate-line (reference line-text)
  (let ((start (1- (tide-plist-get reference :start :offset)))
        (end (1- (tide-plist-get reference :end :offset))))
    (put-text-property start end 'face 'tide-match line-text)
    (put-text-property start end 'mouse-face 'highlight line-text)
    (put-text-property start end 'help-echo "mouse-1: Visit the reference." line-text)
    (put-text-property start end 'tide-reference reference line-text)))

(defun tide-insert-references (references)
  "Create a buffer with the given REFERENCES.

Assumes references are grouped by file name and sorted by line
number."
  (let ((buffer (get-buffer-create "*tide-references*"))
        (inhibit-read-only t)
        (width tab-width)
        (project-root (tide-project-root))
        (last-file-name nil))
    (with-current-buffer buffer
      (erase-buffer)
      (tide-references-mode)
      (setq tab-width width)
      (while references
        (let* ((reference (car references))
               (full-file-name (plist-get reference :file))
               (file-name (file-relative-name full-file-name project-root))
               (line-number (tide-plist-get reference :start :line))
               (line-text (plist-get reference :lineText)))

          ;; file
          (when (not (equal last-file-name file-name))
            (setq last-file-name file-name)
            (insert (propertize file-name 'face 'tide-file))
            (insert "\n"))

          ;; line number
          (insert (propertize (format "%5d" line-number) 'face 'tide-line-number))
          (insert ":")

          ;; line text
          (tide-annotate-line reference line-text)
          (while (-when-let* ((next (cadr references))
                              (full-file-name0 (plist-get next :file))
                              (line-number0 (tide-plist-get next :start :line)))
                   (and (equal full-file-name0 full-file-name) (eq line-number0 line-number)))
            (tide-annotate-line (cadr references) line-text)
            (pop references))
          (insert line-text)

          (insert "\n"))
        (pop references))
      (goto-char (point-min))
      (current-buffer))))

(defun tide-is-identical-reference (original second)
  (and (equal (plist-get original :file) (plist-get second :file))
       (eq (tide-plist-get original :start :line) (tide-plist-get second :start :line))))
(defun tide-find-single-usage (references)
  (let ((definition nil)
        (usage nil)
        (multiple nil))
    (-each references
      #'(lambda (reference)
          (if (eq t (plist-get reference :isDefinition))
              (if (or (eq definition nil) (tide-is-identical-reference definition reference))
                  (setq definition reference)
                (setq multiple t))
            (if (or (eq usage nil) (tide-is-identical-reference usage reference))
                (setq usage reference)
              (setq multiple t)))))
    (if (and (not multiple) usage definition)
        usage
      nil)))

(defun tide-references ()
  "List all references to the symbol at point."
  (interactive)
  (let ((response (tide-command:references)))
    (if (tide-response-success-p response)
        (let ((references (tide-plist-get response :body :refs)))
          (-if-let (usage (tide-find-single-usage references))
              (progn
                (message "This is the only usage.")
                (tide-jump-to-filespan usage nil t))
            (display-buffer (tide-insert-references references))))
      (message (plist-get response :message)))))


;;; Imenu

(defun tide-flatten-navitem (items)
  (if items
      (nconc items (apply #'nconc (-map (lambda (item) (tide-flatten-navitem (plist-get item :childItems))) items)))
    '()))

(defun tide-command:navbar ()
  (tide-send-command-sync "navbar" `(:file ,buffer-file-name)))

(defun tide-imenu-index ()
  (let ((response (tide-command:navbar)))
    (when (tide-response-success-p response)
      (-map
       (lambda (item)
         (cons (concat (plist-get item :text) " " (propertize (plist-get item :kind) 'face 'tide-imenu-type-face))
               (tide-span-to-position (plist-get (car (plist-get item :spans)) :start))))
       (tide-flatten-navitem (plist-get response :body))))))


;;; Rename

(defun tide-command:rename ()
  (tide-send-command-sync "rename" `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))))

(defun tide-rename-symbol-at-location (location new-symbol)
  (let ((file (plist-get location :file)))
    (save-excursion
      (with-current-buffer (find-file-noselect file)
        (-each
            (-map (lambda (filespan)
                    (tide-move-to-location (plist-get filespan :start))
                    (cons (point-marker) filespan))
                  (plist-get location :locs))
          (lambda (pointer)
            (let* ((marker (car pointer))
                   (filespan (cdr pointer)))
              (goto-char marker)
              (delete-char (- (tide-plist-get filespan :end :offset) (tide-plist-get filespan :start :offset)))
              (insert new-symbol))))
        (basic-save-buffer)
        (length (plist-get location :locs))))))

(defun tide-read-new-symbol (old-symbol)
  (let ((new-symbol (read-from-minibuffer (format "Rename %s to: " old-symbol) old-symbol)))
    (if (string-match-p "\\`[ \t\n\r]*\\'" new-symbol)
        (error "Invalid name")
      new-symbol)))

(defun tide-rename-symbol ()
  "Rename symbol at point."
  (interactive)
  (let ((response (tide-command:rename)))
    (tide-on-response-success response
      (if (eq (tide-plist-get response :body :info :canRename) :json-false)
          (message "%s" (tide-plist-get response :body :info :localizedErrorMessage))
        (let* ((old-symbol (tide-plist-get response :body :info :displayName))
               (new-symbol (tide-read-new-symbol old-symbol))
               (locs (tide-plist-get response :body :locs))
               (count 0))
          (cl-flet ((current-file-p (loc)
                                    (file-equal-p (expand-file-name buffer-file-name)
                                                  (plist-get loc :file))))

            ;; Saving current file will trigger a compilation
            ;; check. So make sure all the other files are saved
            ;; before saving current file.

            (-each (nconc (-reject #'current-file-p locs)
                          (-select #'current-file-p locs))
              (lambda (loc)
                (cl-incf count (tide-rename-symbol-at-location loc new-symbol))))

            (message "Renamed %d occurrences." count)))))))

;;; Format

;;;###autoload
(defun tide-format-before-save ()
  "Before save hook to format the buffer before each save."
  (interactive)
  (when (bound-and-true-p tide-mode)
    (tide-format)))

;;;###autoload
(defun tide-format ()
  "Format the current region or buffer."
  (interactive)
  (if (use-region-p)
      (tide-format-region (region-beginning) (region-end))
    (tide-format-region (point-min) (point-max))))

(defun tide-apply-edit (edit)
  (goto-char (tide-location-to-point (plist-get edit :start)))
  (delete-region (point) (tide-location-to-point (plist-get edit :end)))
  (insert (plist-get edit :newText)))

(defun tide-apply-edits (edits)
  (save-excursion
    (-each (nreverse edits)
      (lambda (edit) (tide-apply-edit edit)))))

(defun tide-format-region (start end)
  (let ((response (tide-send-command-sync
                "format"
                `(:file ,buffer-file-name
                  :line ,(tide-line-number-at-pos start)
                  :offset ,(tide-offset start)
                  :endLine ,(tide-line-number-at-pos end)
                  :endOffset ,(tide-offset end)))))
    (tide-on-response-success response
      (tide-apply-edits (plist-get response :body)))))

;;; Mode

(defvar tide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'tide-jump-to-definition)
    (define-key map (kbd "M-,") #'tide-jump-back)
    (define-key map (kbd "C-c d") #'tide-documentation-at-point)
    map))

(defun tide-configure-buffer ()
  (tide-command:openfile)
  (tide-command:configure))

(defun tide-cleanup-buffer ()
  (tide-command:closefile)
  (tide-remove-tmp-file))

;;;###autoload
(defun tide-setup ()
  "Setup `tide-mode' in current buffer."
  (interactive)
  (tide-start-server-if-required)
  (tide-mode 1)
  (set (make-local-variable 'eldoc-documentation-function)
       'tide-eldoc-function)
  (set (make-local-variable 'imenu-auto-rescan) t)
  (set (make-local-variable 'imenu-create-index-function)
       'tide-imenu-index)

  (tide-configure-buffer))

;;;###autoload
(define-minor-mode tide-mode
  "Minor mode for Typescript Interactive Development Environment.

\\{tide-mode-map}"
  :lighter " tide"
  :keymap tide-mode-map
  (if tide-mode
      (progn
        (add-hook 'after-save-hook 'tide-sync-buffer-contents nil t)
        (add-hook 'after-save-hook 'tide-auto-compile-file nil t)
        (add-hook 'after-change-functions 'tide-handle-change nil t)
        (add-hook 'kill-buffer-hook 'tide-cleanup-buffer nil t)
        (add-hook 'hack-local-variables-hook 'tide-configure-buffer nil t)
        (when (commandp 'typescript-insert-and-indent)
          (eldoc-add-command 'typescript-insert-and-indent)))
    (remove-hook 'after-save-hook 'tide-sync-buffer-contents)
    (remove-hook 'after-save-hook 'tide-auto-compile-file)
    (remove-hook 'after-change-functions 'tide-handle-change)
    (remove-hook 'kill-buffer-hook 'tide-cleanup-buffer)
    (remove-hook 'hack-local-variables-hook 'tide-configure-buffer)
    (tide-cleanup-buffer)))


;;; Error highlighting

(defun tide-command:geterr (cb)
  (let* ((result '())
         (resolved nil)
         (err nil))
    (cl-flet
        ((resolve ()
                  (when (not resolved)
                    (if err
                        (progn
                          (setq resolved t)
                          (funcall cb err))
                      (when (and (plist-member result :syntaxDiag)
                                 (plist-member result :semanticDiag))
                        (setq resolved t)
                        (funcall cb `(:body (,result) :success t)))))))
      (tide-send-command
       "syntacticDiagnosticsSync"
       `(:file ,buffer-file-name)
       (lambda (response)
         (if (tide-response-success-p response)
             (setq result (plist-put result :syntaxDiag (plist-get response :body)))
           (setq err response))
         (resolve)))
      (tide-send-command
       "semanticDiagnosticsSync"
       `(:file ,buffer-file-name)
       (lambda (response)
         (if (tide-response-success-p response)
             (setq result (plist-put result :semanticDiag (plist-get response :body)))
           (setq err response))
         (resolve))))))

(defun tide-parse-error (response checker)
  (-map
   (lambda (diagnostic)
     (let* ((start (plist-get diagnostic :start))
            (line (plist-get start :line))
            (column (tide-column line (plist-get start :offset))))
       (flycheck-error-new-at line column 'error (plist-get diagnostic :text)
                              :checker checker)))
   (let ((diagnostic (car (tide-plist-get response :body))))
     (-concat (plist-get diagnostic :syntaxDiag)
              (plist-get diagnostic :semanticDiag)))))

(defun tide-flycheck-send-response (callback checker response)
  (condition-case err
      (funcall callback 'finished (tide-parse-error response checker))
    (error (funcall callback 'errored (error-message-string err)))))

(defun tide-flycheck-start (checker callback)
  (tide-command:geterr
   (lambda (response)
     (if (tide-response-success-p response)
         (tide-flycheck-send-response callback checker response)
       (funcall callback 'errored (plist-get response :message))))))

(defun tide-flycheck-verify (_checker)
  (list
   (flycheck-verification-result-new
    :label "Typescript server"
    :message (if (tide-current-server) "running" "not running")
    :face (if (tide-current-server) 'success '(bold error)))
   (flycheck-verification-result-new
    :label "Tide mode"
    :message (if (bound-and-true-p tide-mode) "enabled" "disabled")
    :face (if (bound-and-true-p tide-mode) 'success '(bold warning)))))

(defun tide-flycheck-predicate ()
  (and (bound-and-true-p tide-mode) (tide-current-server) (not (file-equal-p (tide-project-root) tide-tsserver-directory))))

(flycheck-define-generic-checker 'typescript-tide
  "A TypeScript syntax checker using tsserver."
  :start #'tide-flycheck-start
  :verify #'tide-flycheck-verify
  :modes '(typescript-mode)
  :predicate #'tide-flycheck-predicate)

(add-to-list 'flycheck-checkers 'typescript-tide)
(flycheck-add-next-checker 'typescript-tide '(warning . typescript-tslint) 'append)

(flycheck-define-generic-checker 'javascript-tide
  "A Javascript syntax checker using tsserver."
  :start #'tide-flycheck-start
  :verify #'tide-flycheck-verify
  :modes '(js-mode js2-mode js3-mode)
  :predicate #'tide-flycheck-predicate)

(add-to-list 'flycheck-checkers 'javascript-tide t)

(flycheck-define-generic-checker 'jsx-tide
  "A JSX syntax checker using tsserver."
  :start #'tide-flycheck-start
  :verify #'tide-flycheck-verify
  :modes '(web-mode js2-jsx-mode)
  :predicate #'tide-flycheck-predicate)

(add-to-list 'flycheck-checkers 'jsx-tide t)

(flycheck-define-generic-checker 'tsx-tide
  "A TSX syntax checker using tsserver."
  :start #'tide-flycheck-start
  :verify #'tide-flycheck-verify
  :modes '(web-mode)
  :predicate #'tide-flycheck-predicate)

(add-to-list 'flycheck-checkers 'tsx-tide)

;;; Project errors

(defun tide-command:geterrForProject ()
  (tide-send-command
   "geterrForProject"
   `(:file ,buffer-file-name :delay 0)))

(defun tide-project-errors-buffer-name ()
  (format "*%s-errors*" (tide-project-name)))

(defun tide-display-erros (file-names)
  (with-current-buffer (get-buffer-create (tide-project-errors-buffer-name))
    (tide-project-errors-mode)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (display-buffer (current-buffer) t)
    (let* ((project-files (-filter (lambda (file-name)
                                     (not (string-match-p "node_modules/typescript/" file-name)))
                                   file-names))
           (syntax-remaining-files (copy-list project-files))
           (semantic-remaining-files (copy-list project-files))
           (syntax-errors 0)
           (semantic-errors 0)
           (last-file-name nil))
      (tide-set-event-listener
       (lambda (response)
         (let ((inhibit-read-only t)
               (file-name (tide-plist-get response :body :file))
               (diagnostics (tide-plist-get response :body :diagnostics)))
           (pcase (plist-get response :event)
             ("syntaxDiag"
              (progn
                (setq syntax-remaining-files (remove file-name syntax-remaining-files))
                (incf syntax-errors (length diagnostics))))
             ("semanticDiag"
              (progn
                (setq semantic-remaining-files (remove file-name semantic-remaining-files))
                (incf semantic-errors (length diagnostics)))))

           (when diagnostics
             (-each diagnostics
               (lambda (diagnostic)
                 (let ((line-number (tide-plist-get diagnostic :start :line)))
                   (when (not (equal last-file-name file-name))
                     (setq last-file-name file-name)
                     (insert (propertize (file-relative-name file-name (tide-project-root)) 'face 'tide-file))
                     (insert "\n"))

                   (insert (propertize (format "%5d" line-number) 'face 'tide-line-number 'tide-error (plist-put diagnostic :file file-name)))
                   (insert ": ")
                   (insert (plist-get diagnostic :text))
                   (insert "\n")))))
           (when (and (null syntax-remaining-files) (null semantic-remaining-files))
             (insert (format "\n%d syntax error(s), %d semantic error(s)\n" syntax-errors semantic-errors))
             (goto-char (point-min))
             (tide-clear-event-listener)))))))
  (tide-command:geterrForProject))

(defun tide-next-error-function (n &optional reset)
  "Override for `next-error-function' for use in tide-project-errors-mode buffers."
  (interactive "p")

  (-when-let (buffer (get-buffer (tide-project-errors-buffer-name)))
    (with-current-buffer buffer
      (when reset
        (goto-char (point-min)))
      (if (> n 0)
          (tide-find-next-error (point) n)
        (tide-find-previous-error (point) (- n)))
      (tide-goto-error))))

(defun tide-find-next-error (pos arg)
  "Move to next error."
  (interactive "d\np")
  (setq arg (* 2 arg))
  (unless (get-text-property pos 'tide-error)
    (setq arg (1- arg)))
  (dotimes (_i arg)
    (setq pos (next-single-property-change pos 'tide-error))
    (unless pos
      (error "Moved past last error")))
  (goto-char pos))

(defun tide-find-previous-error (pos arg)
  "Move back to previous error."
  (interactive "d\np")
  (dotimes (_i (* 2 arg))
    (setq pos (previous-single-property-change pos 'tide-error))
    (unless pos
      (error "Moved back before first error")))
  (goto-char pos))

(defun tide-goto-error ()
  "Jump to error location in the file."
  (interactive)
  (-when-let (error (get-text-property (point) 'tide-error))
    (tide-jump-to-filespan error nil t)))

(defvar tide-project-errors-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tide-find-next-error)
    (define-key map (kbd "p") #'tide-find-previous-error)
    (define-key map (kbd "C-m") #'tide-goto-error)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode tide-project-errors-mode nil "tide-project-errors"
  "Major mode for tide project-errors list.

\\{tide-project-errors-mode-map}"
  (use-local-map tide-project-errors-mode-map)
  (setq buffer-read-only t)
  (setq next-error-function #'tide-next-error-function))

;;;###autoload
(defun tide-project-errors ()
  (interactive)
  (tide-command:projectInfo
   (lambda (response)
     (tide-on-response-success response
       (tide-display-erros (tide-plist-get response :body :fileNames))))
   t))

;;; Identifier highlighting

(defun tide-command:documentHighlights ()
  (tide-send-command-sync
   "documentHighlights"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset) :filesToSearch (,buffer-file-name))))

(defface tide-hl-identifier-face
  '((t (:inherit highlight)))
  "Face used for highlighting identifiers in `tide-hl-identifier'."
  :group 'tide)

(defcustom tide-hl-identifier-idle-time 0.1
  "How long to wait after user input before highlighting the current identifier."
  :type 'float
  :group 'tide)

(defvar tide--current-hl-identifier-idle-time
  0
  "The current delay for hl-identifier-mode.")

(defvar tide--hl-identifier-timer
  nil
  "The global timer used for highlighting identifiers.")

;;;###autoload
(defun tide-unhighlight-identifiers ()
  "Remove highlights from previously highlighted identifier."
  (remove-overlays nil nil 'tide-overlay 'sameid))

;;;###autoload
(defun tide-hl-identifier ()
  "Highlight all instances of the identifier under point. Removes
highlights from previously highlighted identifier."
  (interactive)
  (tide-unhighlight-identifiers)
  (tide--hl-identifier))

(defun tide--hl-identifier ()
  "Highlight all instances of the identifier under point."
  (let ((response (tide-command:documentHighlights)))
    (when (tide-response-success-p response)
      (let ((references (plist-get (car (plist-get (tide-command:documentHighlights) :body)) :highlightSpans)))
        (-each references
          (lambda (reference)
            (let* ((kind (plist-get reference :kind))
                   (id-start (plist-get reference :start))
                   (id-end (plist-get reference :end)))
              (when (member kind '("reference" "writtenReference"))
                (let ((x (make-overlay (tide-location-to-point id-start) (tide-location-to-point id-end))))
                  (overlay-put x 'tide-overlay 'sameid)
                  (overlay-put x 'face 'tide-hl-identifier-face))))))))))

(defun tide--hl-identifiers-function ()
  "Function run after an idle timeout, highlighting the
identifier at point, if necessary."
  (when tide-hl-identifier-mode
    (unless (tide--on-overlay-p 'sameid)
	  (tide-hl-identifier))
    (unless (eq tide--current-hl-identifier-idle-time tide-hl-identifier-idle-time)
      (tide--hl-set-timer))))

(defun tide--hl-set-timer ()
  (if tide--hl-identifier-timer
      (cancel-timer tide--hl-identifier-timer))
  (setq tide--current-hl-identifier-idle-time tide-hl-identifier-idle-time)
  (setq tide--hl-identifier-timer (run-with-idle-timer
				      tide-hl-identifier-idle-time
				      t
				      #'tide--hl-identifiers-function)))

;;;###autoload
(define-minor-mode tide-hl-identifier-mode
  "Highlight instances of the identifier at point after a short
timeout."
  :group 'tide
  (if tide-hl-identifier-mode
      (progn
	(tide--hl-set-timer)
	;; Unhighlight if point moves off identifier
	(add-hook 'post-command-hook #'tide--hl-identifiers-post-command-hook nil t)
	;; Unhighlight any time the buffer changes
	(add-hook 'before-change-functions #'tide--hl-identifiers-before-change-function nil t))
    (remove-hook 'post-command-hook #'tide--hl-identifiers-post-command-hook t)
    (remove-hook 'before-change-functions #'tide--hl-identifiers-before-change-function t)
    (tide-unhighlight-identifiers)))

(defun tide--on-overlay-p (id)
  "Return whether point is on a tide overlay of type ID."
  (cl-find-if (lambda (el) (eq (overlay-get el 'tide-overlay) id)) (overlays-at (point))))

(defun tide--hl-identifiers-post-command-hook ()
  (if (and tide-hl-identifier-mode
	   (not (tide--on-overlay-p 'sameid)))
      (tide-unhighlight-identifiers)))

(defun tide--hl-identifiers-before-change-function (_beg _end)
  (tide-unhighlight-identifiers))


;;; Compile On Save

(defvar tide-project-configs (make-hash-table :test 'equal))

(defun tide-command:compileOnSaveEmitFile ()
  (tide-send-command "compileOnSaveEmitFile" `(:file ,buffer-file-name)))

(defun tide-compile-file ()
  "Compiles the current file"
  (interactive)
  (tide-command:compileOnSaveEmitFile))

(defun tide-auto-compile-file ()
  "Compiles the current file if compileOnSave is set"
  (interactive)
  (tide-project-config
   (lambda (config)
     (when (and config (eq (plist-get config :compileOnSave) t))
       (tide-command:compileOnSaveEmitFile)))))

(defun tide-project-config (cb)
  (let ((config (gethash (tide-project-name) tide-project-configs :not-loaded)))
    (if (eq config :not-loaded)
        (tide-command:projectInfo
         (lambda (response)
           (tide-on-response-success response
             (let* ((config-file-name (tide-plist-get response :body :configFileName))
                    (config (and config-file-name (file-exists-p config-file-name) (json-read-file config-file-name))))
               (puthash (tide-project-name) config tide-project-configs)
               (funcall cb config)))))
      (funcall cb config))))

;;; Utility commands

(defun tide-restart-server ()
  "Restarts tsserver."
  (interactive)
  (-when-let (server (tide-current-server))
    (delete-process server))
  (tide-start-server)
  (tide-each-buffer (tide-project-name) #'tide-configure-buffer))

(provide 'tide)

;;; tide.el ends here
