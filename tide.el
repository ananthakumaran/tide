;;; tide.el --- Typescript Interactive Development Environment -*- lexical-binding: t -*-

;; Copyright (C) 2015 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; URL: http://github.com/ananthakumaran/tide
;; Version: 1.6.2
;; Keywords: typescript
;; Package-Requires: ((dash "2.10.0") (flycheck "0.23") (emacs "24.1") (typescript-mode "0.1"))

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
(require 'cl)
(require 'eldoc)
(require 'dash)
(require 'flycheck)
(require 'imenu)

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

(defmacro tide-def-permanent-buffer-local (name &optional init-value)
  "Declare NAME as buffer local variable."
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

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
   (let ((root (locate-dominating-file default-directory "tsconfig.json")))
     (unless root
       (error "Couldn't locate project root folder.  Please make sure to add tsconfig.json in project root folder"))
     (let ((full-path (expand-file-name root)))
       (setq tide-project-root full-path)
       full-path))))

(defun tide-project-name ()
  (file-name-nondirectory (directory-file-name (tide-project-root))))

;;; Helpers

(defun tide-plist-get (list &rest args)
  (reduce
   (lambda (object key)
     (when object
       (plist-get object key)))
   args
   :initial-value list))

(defun tide-response-success-p (response)
  (and response (equal (plist-get response :success) t)))

(defun tide-join (list)
  (mapconcat 'identity list ""))

(defun tide-each-buffer (project-name fn)
  "Callback FN for each buffer within PROJECT-NAME with tide-mode enabled."
  (-each (buffer-list)
    (lambda (buffer)
      (with-current-buffer buffer
        (when (and (boundp tide-mode)
                   tide-mode
                   (equal (tide-project-name) project-name))
          (funcall fn))))))

(defun tide-current-offset ()
  "Number of characters present from the begining of line to cursor in current line.

offset is one based."
  (1+ (- (point) (line-beginning-position))))

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
        (decf offset))
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
    (erase-buffer)
    (when string
      (save-excursion
        (insert string)))
    (current-buffer)))

;;; Server

(defun tide-current-server ()
  (gethash (tide-project-name) tide-servers))

(defun tide-next-request-id ()
  (number-to-string (incf tide-request-counter)))

(defun tide-dispatch-response (response)
  (let* ((request-id (plist-get response :request_seq))
         (callback (gethash request-id tide-response-callbacks)))
    (when callback
      (with-current-buffer (car callback)
        (apply (cdr callback) (list response)))
      (remhash request-id tide-response-callbacks))))

(defun tide-dispatch (response)
  (case (intern (plist-get response :type))
    ('response (tide-dispatch-response response))))

(defun tide-send-command (name args &optional callback)
  (when (not (tide-current-server))
    (error "Server does not exists.  Run M-x tide-restart-server to start it again"))

  (when tide-buffer-dirty
    (tide-sync-buffer-contents))

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
    (message "(%s) tsserver exists: %s." project-name (string-trim message))
    (ignore-errors
      (kill-buffer (process-buffer process)))
    (tide-cleanup-project project-name)))

(defun tide-start-server ()
  (when (tide-current-server)
    (error "Server already exists"))

  (message "(%s) Starting tsserver..." (tide-project-name))
  (let* ((default-directory (tide-project-root))
         (process-environment (append tide-tsserver-process-environment process-environment))
         (buf (generate-new-buffer tide-server-buffer-name))
         (process (start-file-process "tsserver" buf "node" (expand-file-name "tsserver.js" tide-tsserver-directory))))
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (set-process-filter process #'tide-net-filter)
    (set-process-sentinel process #'tide-net-sentinel)
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
  (remhash project-name tide-servers))

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

(defun tide-command:configure ()
  (tide-send-command "configure" `(:hostInfo ,(emacs-version) :file ,buffer-file-name :formatOptions (:tabSize ,tab-width :indentSize ,typescript-indent-level :convertTabToSpaces ,(not indent-tabs-mode)))))

(defun tide-command:openfile ()
  (tide-send-command "open" `(:file ,buffer-file-name)))

(defun tide-command:closefile ()
  (tide-send-command "close" `(:file ,buffer-file-name)))

;;; Jump to definition

(defun tide-command:definition (cb)
  (tide-send-command
   "definition"
   `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(tide-current-offset))
   cb))

(defun tide-command:type-definition (cb)
  (tide-send-command
   "typeDefinition"
   `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(tide-current-offset))
   cb))

(defun tide-jump-to-definition (&optional arg)
  "Jump to the definition of the symbol at point.

With a prefix arg, Jump to the type definition."
  (interactive "P")
  (let ((cb (lambda (response)
              (when (tide-response-success-p response)
                (let ((filespan (car (plist-get response :body))))
                  (tide-jump-to-filespan filespan t))))))
    (if arg
        (tide-command:type-definition cb)
      (tide-command:definition cb))))

(defun tide-move-to-span (span)
  (let* ((line (plist-get span :line))
         (offset (plist-get span :offset)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))
    (forward-char (1- offset))))

(defun tide-jump-to-filespan (filespan &optional reuse-window no-marker)
  (let ((file (plist-get filespan :file)))
    (unless no-marker
      (ring-insert find-tag-marker-ring (point-marker)))
    (if reuse-window
        (pop-to-buffer (find-file-noselect file) '((display-buffer-reuse-window display-buffer-same-window)))
      (pop-to-buffer (find-file-noselect file)))
    (tide-move-to-span (plist-get filespan :start))))

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
           `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(tide-current-offset)))))
    (when (tide-response-success-p response)
      (tide-annotate-signatures (plist-get response :body)))))

(defun tide-method-call-p ()
  (or (looking-at "[(,]") (and (not (looking-at "\\sw")) (looking-back "[(,]\n?\\s-*"))))

(defun tide-command:quickinfo ()
  (let ((response (tide-send-command-sync "quickinfo" `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(tide-current-offset)))))
    (when (tide-response-success-p response)
      response)))

(defun tide-eldoc-function ()
  (when (not (member last-command '(next-error previous-error)))
    (if (tide-method-call-p)
        (tide-command:signatureHelp)
      (when (looking-at "\\s_\\|\\sw")
        (-when-let (quick-info (tide-command:quickinfo))
          (tide-plist-get quick-info :body :displayString))))))


(defun tide-documentation-at-point ()
  "Show documentation of the symbol at point."
  (interactive)
  (let ((documentation
         (-when-let* ((quick-info (tide-command:quickinfo))
                      (display-string (tide-plist-get quick-info :body :displayString))
                      (documentation (tide-plist-get quick-info :body :documentation)))
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
  (setq tide-buffer-dirty nil)
  (when (not tide-buffer-tmp-file)
    (setq tide-buffer-tmp-file (make-temp-file "tide")))
  (write-region (point-min) (point-max) tide-buffer-tmp-file nil 'no-message)
  (tide-send-command "reload" `(:file ,buffer-file-name :tmpfile ,tide-buffer-tmp-file)))


;;; Auto completion

(defun tide-completion-annotation (completion)
  (pcase (plist-get completion :kind)
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
    (t nil)))

(defun tide-completion-prefix ()
  (company-grab-symbol-cons "\\." 1))

(defun tide-member-completion-p (prefix)
  (save-excursion
    (backward-char (length prefix))
    (equal (string (char-before (point))) ".")))

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
          `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(- (tide-current-offset) (length prefix)))))
    (when (not (tide-member-completion-p prefix))
      (plist-put file-location :prefix prefix))
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
             (derived-mode-p 'typescript-mode)
             (tide-current-server)
             (not (company-in-string-or-comment))
             (or (tide-completion-prefix) 'stop)))
    (candidates (cons :async
                      (lambda (cb)
                        (tide-command:completions arg cb))))
    (sorted t)
    (meta (tide-completion-meta arg))
    (annotation (tide-completion-annotation (get-text-property 0 'completion arg)))
    (doc-buffer (tide-completion-doc-buffer arg))))

(eval-after-load 'company
  '(progn
     (pushnew 'company-tide company-backends)))

;;; References

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
    map))

(define-derived-mode tide-references-mode nil "tide-references"
  "Major mode for tide references list.

\\{tide-references-mode-map}"
  (use-local-map tide-references-mode-map)
  (setq buffer-read-only t))

(defun tide-command:references ()
  (tide-send-command-sync
   "references"
   `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(tide-current-offset))))

(defun tide-annotate-line (reference line-text)
  (let ((start (1- (tide-plist-get reference :start :offset)))
        (end (1- (tide-plist-get reference :end :offset))))
    (put-text-property start end 'face 'tide-match line-text)
    (put-text-property start end 'tide-reference reference line-text)))

(defun tide-insert-references (references)
  "Create a buffer with the give REFERENCES.

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

(defun tide-references ()
  "List all references to the symbol at point."
  (interactive)
  (let ((response (tide-command:references)))
    (if (tide-response-success-p response)
        (display-buffer (tide-insert-references (tide-plist-get response :body :refs)))
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
  (tide-send-command-sync "rename" `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(tide-current-offset))))

(defun tide-rename-symbol-at-location (location new-symbol)
  (let ((file (plist-get location :file)))
    (save-excursion
      (with-current-buffer (find-file-noselect file)
        (-each
            (-map (lambda (filespan)
                    (tide-move-to-span (plist-get filespan :start))
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
  (let ((new-symbol (read-from-minibuffer (format "Rename %s to " old-symbol))))
    (if (string-blank-p new-symbol)
        (error "Invalid name")
      new-symbol)))

(defun tide-rename-symbol ()
  "Rename symbol at point."
  (interactive)
  (let ((response (tide-command:rename)))
    (when (tide-response-success-p response)
      (if (eq (tide-plist-get response :body :info :canRename) :json-false)
          (message "%s" (tide-plist-get response :body :info :localizedErrorMessage))
        (let* ((old-symbol (tide-plist-get response :body :info :fullDisplayName))
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
                (incf count (tide-rename-symbol-at-location loc new-symbol))))

            (message "Renamed %d occurrences." count)))))))

;;; Mode

(defvar tide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'tide-jump-to-definition)
    (define-key map (kbd "M-,") #'tide-jump-back)
    (define-key map (kbd "C-c d") #'tide-documentation-at-point)
    map))

(defun tide-configure-buffer ()
  (tide-command:configure)
  (tide-command:openfile))

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

  ;; Call configure command right away if called interactively, all
  ;; the local variables should be set by this time.
  (when (called-interactively-p 'interactive)
    (tide-configure-buffer)))

;;;###autoload
(define-minor-mode tide-mode
  "Minor mode for Typescript Interactive Development Environment.

\\{tide-mode-map}"
  :lighter " tide"
  :keymap tide-mode-map
  (if tide-mode
      (progn
        (add-hook 'after-save-hook 'tide-sync-buffer-contents nil t)
        (add-hook 'after-change-functions 'tide-handle-change nil t)
        (add-hook 'kill-buffer-hook 'tide-cleanup-buffer nil t)
        (add-hook 'hack-local-variables-hook 'tide-configure-buffer nil t)
        (when (commandp 'typescript-insert-and-indent)
          (eldoc-add-command 'typescript-insert-and-indent)))
    (remove-hook 'after-save-hook 'tide-sync-buffer-contents)
    (remove-hook 'after-change-functions 'tide-handle-change)
    (remove-hook 'kill-buffer-hook 'tide-cleanup-buffer)
    (remove-hook 'hack-local-variables-hook 'tide-configure-buffer)
    (tide-cleanup-buffer)))


;;; Error highlighting

(defun tide-command:geterr (cb)
  (tide-send-command
   "geterr"
   `(:responseType "response" :files (,buffer-file-name))
   cb))

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
    :message (if tide-mode "enabled" "disabled")
    :face (if tide-mode 'success '(bold warning)))))

(flycheck-define-generic-checker 'typescript-tide
  "A syntax checker for Typescript using Tide Mode."
  :start #'tide-flycheck-start
  :verify #'tide-flycheck-verify
  :modes '(typescript-mode)
  :predicate (lambda () (and tide-mode (tide-current-server) (not (file-equal-p (tide-project-root) tide-tsserver-directory)))))

(add-to-list 'flycheck-checkers 'typescript-tide)

;;; Utility commands

(defun tide-restart-server ()
  "Restarts tsserver."
  (interactive)
  (-when-let (server (tide-current-server))
    (delete-process server))
  (tide-start-server)
  (tide-each-buffer (tide-project-name) #'tide-configure-buffer))

(provide 'tide)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; tide.el ends here
