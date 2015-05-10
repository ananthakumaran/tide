;;; tide.el --- Typescript Interactive Development Environment -*- lexical-binding: t -*-

;; Copyright (C) 2015 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; URL: http://github.com/ananthakumaran/tide
;; Version: 0.1
;; Keywords: typescript
;; Package-Requires: ((dash "2.10.0") (flycheck "0.23") (emacs "24.1"))

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

(require 'etags)
(require 'json)
(require 'cl)
(require 'eldoc)
(require 'dash)
(require 'flycheck)
(require 'typescript)

(defgroup tide nil
  "TypeScript Interactive Development Environment."
  :prefix "tide-"
  :group 'tools)

(defcustom tide-tsserver-executable
  "tsserver"
  "Typescript server executable path."
  :type 'string
  :group 'tide)

(defcustom tide-sync-request-timeout 2
  "The number of seconds to wait for a sync response."
  :type 'integer
  :group 'tide)

(defmacro tide-def-permanent-buffer-local (name &optional init-value)
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defvar tide-server-buffer-name "*tide-server*")
(defvar tide-request-counter 0)

(tide-def-permanent-buffer-local tide-project-root nil)
(tide-def-permanent-buffer-local tide-buffer-dirty nil)
(tide-def-permanent-buffer-local tide-buffer-tmp-file nil)
(tide-def-permanent-buffer-local tide-event-queue '())

(defvar tide-servers (make-hash-table :test 'equal))
(defvar tide-response-callbacks (make-hash-table :test 'equal))

(defun tide-project-root ()
  (or
   tide-project-root
   (let ((root (locate-dominating-file default-directory "tsconfig.json")))
     (unless root
       (error "Couldn't locate project root"))
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

(defun tide-current-offset ()
  (let ((p0 (point))
        (offset 1))
    (save-excursion
      (beginning-of-line)
      (while (< (point) p0)
        (forward-char)
        (incf offset))
      offset)))

(defun tide-column (line offset)
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

(defun tide-dispatch-event (response)
  (let* ((file (tide-plist-get response :body :file))
         (buffer (get-file-buffer file)))
    (when buffer
      (with-current-buffer buffer
        (-when-let (cb (pop tide-event-queue))
          (apply cb (list response)))))))

(defun tide-dispatch (response)
  (case (intern (plist-get response :type))
    ('response (tide-dispatch-response response))
    ('event (tide-dispatch-event response))))

(defun tide-send-command (name args &optional callback)
  (when (not (tide-current-server))
    (error "Server does not exists"))

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
        (error "sync request timed out %s" name)))
    response))

(defun tide-net-filter (process data)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert data))
  (tide-decode-response process))

(defun tide-net-sentinel (process message)
  (message "tss server exists: %s." message)
  (with-current-buffer (process-buffer process)
    (remhash (tide-project-name) tide-servers))
  (kill-buffer (process-buffer process)))

(defun tide-start-server ()
  (when (tide-current-server)
    (error "Server already exists"))

  (message "Starting tsserver...")
  (let* ((default-directory (tide-project-root))
         (process-environment (append '("TSS_LOG=-level verbose") process-environment))
         (buf (get-buffer-create tide-server-buffer-name))
         (process (start-file-process "tsserver" buf tide-tsserver-executable)))
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (set-process-filter process #'tide-net-filter)
    (set-process-sentinel process #'tide-net-sentinel)
    (puthash (tide-project-name) process tide-servers)
    (message "tsserver server started successfully.")))

(defun tide-start-server-if-required ()
  (when (not (tide-current-server))
    (tide-start-server)))

(defun tide-decode-response-legth ()
  (goto-char (point-min))
  (when (re-search-forward "Content-Length: \\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun tide-enough-response-p (length)
  (>= (- (position-bytes (point-max)) (position-bytes (point))) (+ length 1)))

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

(defun tide-command:definition ()
  "Jump to definition at point."
  (interactive)
  (tide-send-command
   "definition"
   `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(tide-current-offset))
   (lambda (response)
     (when (tide-response-success-p response)
       (let* ((filespan (car (plist-get response :body))))
         (tide-jump-to-filespan filespan))))))

(defun tide-jump-to-filespan (filespan)
  (let* ((file (plist-get filespan :file))
         (line (tide-plist-get filespan :start :line))
         (offset (- (tide-plist-get filespan :start :offset) 1)))
    (ring-insert find-tag-marker-ring (point-marker))
    (pop-to-buffer (find-file-noselect file) '((display-buffer-reuse-window display-buffer-same-window)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))
    (forward-char offset)))


;;; Eldoc

(defun tide-annotate-display-part (display-part)
  (plist-get display-part :text))

(defun tide-annotate-signature-parameter (parameter)
  (tide-join (-map #'tide-annotate-display-part (plist-get parameter :displayParts))))

(defun tide-annotate-signature (signature)
  (let ((separator (tide-join (-map #'tide-annotate-display-part (plist-get signature :separatorDisplayParts)))))
    (tide-join
     (-concat
      (-map #'tide-annotate-display-part (plist-get signature :prefixDisplayParts))
      (list
       (-if-let (params (plist-get signature :parameters))
           (mapconcat #'tide-annotate-signature-parameter params separator)
         ""))
      (-map #'tide-annotate-display-part (plist-get signature :suffixDisplayParts))))))

(defun tide-annotate-signatures (body)
  (mapconcat #'identity (-map #'tide-annotate-signature (plist-get body :items)) "\n"))

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
      (tide-plist-get response :body :displayString))))

(defun tide-eldoc-function ()
  (when (not (member last-command '(next-error previous-error)))
    (if (tide-method-call-p)
        (tide-command:signatureHelp)
      (when (looking-at "\\s_\\|\\sw")
        (tide-command:quickinfo)))))

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

(defun tide-command:completion-entry-details (name)
  (let ((arguments (-concat (get-text-property 0 'file-location name)
                            `(:entryNames (,name)))))
    (-when-let (response (tide-send-command-sync "completionEntryDetails" arguments))
      (when (tide-response-success-p response)
        (-when-let (detail (car (plist-get response :body)))
          (tide-join
           (-map (lambda (part) (plist-get part :text)) (plist-get detail :displayParts))))))))

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
    (meta (tide-command:completion-entry-details arg))))

;;; Error

(defun tide-command:geterr ()
  (tide-send-command "geterr"
                     `(:delay 0 :files (,buffer-file-name))))

(defun tide-parse-error (event checker)
  (-map
   (lambda (diagnostic)
     (let* ((start (plist-get diagnostic :start))
            (line (plist-get start :line))
            (column (tide-column line (plist-get start :offset))))
       (flycheck-error-new-at line column 'error (plist-get diagnostic :text)
                              :checker checker)))
   (tide-plist-get event :body :diagnostics)))

(defun tide-flycheck-send-response (callback checker events)
  (condition-case err
      (funcall callback 'finished (-mapcat (lambda (event) (tide-parse-error event checker)) events))
    (error (funcall callback 'errored (error-message-string err)))))

(defun tide-flycheck-start (checker callback)
  (let* ((response-count 0)
         (events '())
         (error-collector (lambda (event)
                            (push event events)
                            (incf response-count)
                            (when (eql response-count 2)
                              (tide-flycheck-send-response callback checker events)))))
    (setq tide-event-queue
          (nconc tide-event-queue (list error-collector error-collector)))
    (tide-command:geterr)))

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
  :predicate (lambda () (and tide-mode (tide-current-server))))

(add-to-list 'flycheck-checkers 'typescript-tide)

;;; Mode

(defvar tide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'tide-command:definition)
    map))

(defun tide-configure-buffer ()
  (tide-command:configure)
  (tide-command:openfile))

(defun tide-cleanup-buffer ()
  (tide-command:closefile)
  (tide-remove-tmp-file))

(defun tide-setup ()
  (interactive)
  (tide-start-server-if-required)
  (tide-mode 1)
  (set (make-local-variable 'eldoc-documentation-function)
       'tide-eldoc-function))

;;;###autoload
(define-minor-mode tide-mode
  "Minor mode for Typescript Interactive Development Environment.

\\{tide-mode-map}"
  :lighter " tide"
  :keymap tide-mode-map
  (if tide-mode
      (progn
        (add-hook 'after-save-hook 'tide-command:reloadfile nil t)
        (add-hook 'after-change-functions 'tide-handle-change nil t)
        (add-hook 'kill-buffer-hook 'tide-cleanup-buffer nil t)
        (add-hook 'hack-local-variables-hook 'tide-configure-buffer nil t)
        (when (commandp 'typescript-insert-and-indent)
          (eldoc-add-command 'typescript-insert-and-indent)))
    (remove-hook 'after-save-hook 'tide-command:reloadfile)
    (remove-hook 'after-change-functions 'tide-handle-change)
    (remove-hook 'kill-buffer-hook 'tide-cleanup-buffer)
    (remove-hook 'hack-local-variables-hook 'tide-configure-buffer)
    (tide-cleanup-buffer)))

(provide 'tide)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; tide.el ends here
