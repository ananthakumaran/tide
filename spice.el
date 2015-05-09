;;; spice.el --- Typescript Interactive Development Environment -*- lexical-binding: t -*-

;; Copyright (C) 2015 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; URL: http://github.com/ananthakumaran/spice
;; Version: 0.1
;; Keywords: typescript
;; Package-Requires: ((dash "2.10.0"))

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

(defgroup spice nil
  "TypeScript Interactive Development Environment."
  :prefix "spice-"
  :group 'tools)

(defcustom spice-tsserver-executable
  "tsserver"
  "Typescript server executable path."
  :type 'string
  :group 'spice)

(defcustom spice-sync-request-timeout 2
  "The number of seconds to wait for a sync response."
  :type 'integer
  :group 'spice)

(defmacro spice-def-permanent-buffer-local (name &optional init-value)
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defvar spice-server-buffer-name "*spice-server*")
(defvar spice-request-counter 0)

(spice-def-permanent-buffer-local spice-project-root nil)
(spice-def-permanent-buffer-local spice-buffer-dirty nil)
(spice-def-permanent-buffer-local spice-buffer-tmp-file nil)

(defvar spice-servers (make-hash-table :test 'equal))
(defvar spice-response-callbacks (make-hash-table :test 'equal))

(defun spice-project-root ()
  (or
   spice-project-root
   (let ((root (locate-dominating-file default-directory "tsconfig.json")))
     (unless root
       (error "Couldn't locate project root"))
     (let ((full-path (expand-file-name root)))
       (setq spice-project-root full-path)
       full-path))))

(defun spice-project-name ()
  (file-name-nondirectory (directory-file-name (spice-project-root))))

;;; helpers

(defun spice-plist-get (list &rest args)
  (reduce
   (lambda (object key)
     (when object
       (plist-get object key)))
   args
   :initial-value list))

(defun spice-response-success-p (response)
  (and response (equal (plist-get response :success) t)))

;;; server

(defun spice-current-server ()
  (gethash (spice-project-name) spice-servers))

(defun spice-next-request-id ()
  (number-to-string (incf spice-request-counter)))

(defun spice-dispatch (response source-buffer)
  (let* ((request-id (plist-get response :request_seq))
         (callback (gethash request-id spice-response-callbacks)))
    (when callback
      (with-current-buffer source-buffer
        (apply callback (list response)))
      (remhash request-id spice-response-callbacks))))

(defun spice-send-command (name args &optional callback)
  (when (not (spice-current-server))
    (error "Server does not exists"))

  (when spice-buffer-dirty
    (spice-sync-buffer-contents))

  (let* ((request-id (spice-next-request-id))
         (command `(:command ,name :seq ,request-id :arguments ,args))
         (encoded-command (json-encode command))
         (payload (concat encoded-command "\n")))
    (process-send-string (spice-current-server) payload)
    (when callback
      (puthash request-id callback spice-response-callbacks)
      (accept-process-output nil 0.01))))

(defun spice-send-command-sync (name args)
  (let* ((start-time (current-time))
         (response nil))
    (spice-send-command name args (lambda (resp) (setq response resp)))
    (while (not response)
      (accept-process-output nil 0.01)
      (when (> (cadr (time-subtract (current-time) start-time))
               spice-sync-request-timeout)
        (error "sync request timed out %s" name)))
    response))

(defun spice-net-filter (process data)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert data))
  (spice-decode-response process))

(defun spice-net-sentinel (process message)
  (message "tss server exists: %s." message)
  (with-current-buffer (process-buffer process)
    (remhash (spice-project-name) spice-servers))
  (kill-buffer (process-buffer process)))

(defun spice-start-server ()
  (when (spice-current-server)
    (error "Server already exists"))

  (message "Starting tsserver...")
  (let* ((default-directory (spice-project-root))
         (process-environment (append '("TSS_LOG=-level verbose") process-environment))
         (buf (get-buffer-create spice-server-buffer-name))
         (process (start-file-process "tsserver" buf spice-tsserver-executable)))
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (set-process-filter process #'spice-net-filter)
    (set-process-sentinel process #'spice-net-sentinel)
    (puthash (spice-project-name) process spice-servers)
    (message "tsserver server started successfully.")))

(defun spice-start-server-if-required ()
  (when (not (spice-current-server))
    (spice-start-server)))

(defun spice-decode-response-legth ()
  (goto-char (point-min))
  (when (re-search-forward "Content-Length: \\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun spice-enough-response-p (length)
  (>= (- (position-bytes (point-max)) (position-bytes (point))) (+ length 1)))

(defun spice-decode-response (process)
  (let ((source-buffer (current-buffer)))
    (with-current-buffer (process-buffer process)
      (let ((length (spice-decode-response-legth))
            (json-object-type 'plist)
            (json-array-type 'list))
        (when (and length (spice-enough-response-p length))
          (spice-dispatch
           (prog2
               (progn
                 (search-forward "{")
                 (backward-char 1))
               (json-read-object)
             (delete-region (point-min) (point)))
           source-buffer)
          (when (>= (buffer-size) 16)
            (spice-decode-response process)))))))

;;; Initialization

(defun spice-command:configure ()
  (interactive)
  (spice-send-command "configure" `(:hostInfo ,(emacs-version) :file ,buffer-file-name :formatOptions (:tabSize ,tab-width :convertTabToSpaces ,nil))))

(defun spice-command:openfile ()
  (interactive)
  (spice-send-command "open" `(:file ,buffer-file-name)))

;;; Jump to definitions

(defun spice-command:definition ()
  "Jump to definition at point."
  (interactive)
  (spice-send-command
   "definition"
   `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(current-column))
   (lambda (response)
     (when (spice-response-success-p response)
       (let* ((filespan (car (plist-get response :body))))
         (spice-jump-to-filespan filespan))))))

(defun spice-jump-to-filespan (filespan)
  (let* ((file (plist-get filespan :file))
         (line (spice-plist-get filespan :start :line))
         (offset (- (spice-plist-get filespan :start :offset) 1)))
    (ring-insert find-tag-marker-ring (point-marker))
    (pop-to-buffer (find-file-noselect file) '((display-buffer-reuse-window display-buffer-same-window)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))
    (move-to-column offset)))


;;; Eldoc

(defun spice-command:quickinfo ()
  (let ((response (spice-send-command-sync "quickinfo" `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(current-column)))))
    (when (spice-response-success-p response)
      (spice-plist-get response :body :displayString))))

(defun spice-eldoc-function ()
  (when (not (member last-command '(next-error previous-error)))
    (spice-command:quickinfo)))

;;; Buffer Sync

(defun spice-remove-tmp-file ()
  (when spice-buffer-tmp-file
    (delete-file spice-buffer-tmp-file)
    (setq spice-buffer-tmp-file nil)))

(defun spice-command:reloadfile ()
  (interactive)
  (spice-send-command "reload" `(:file ,buffer-file-name :tmpfile ,buffer-file-name)))

(defun spice-handle-change (_beg _end _len)
  (setq spice-buffer-dirty t))

(defun spice-sync-buffer-contents ()
  (setq spice-buffer-dirty nil)
  (when (not spice-buffer-tmp-file)
    (setq spice-buffer-tmp-file (make-temp-file "spice")))
  (write-region (point-min) (point-max) spice-buffer-tmp-file nil 'no-message)
  (spice-send-command "reload" `(:file ,buffer-file-name :tmpfile ,spice-buffer-tmp-file)))


;;; Auto completion

(defun spice-completion-prefix ()
  (company-grab-symbol-cons "\\." 1))

(defun spice-member-completion-p (prefix)
  (save-excursion
    (backward-char (length prefix))
    (equal (string (char-before (point))) ".")))

(defun spice-annotate-completions (completions prefix file-location)
  (-map
   (lambda (completion)
     (let ((name (plist-get completion :name)))
       (put-text-property 0 1 'file-location file-location name)
       name))
   (-filter
    (lambda (completion)
      (string-prefix-p prefix (plist-get completion :name)))
    completions)))

(defun spice-command:completions (prefix cb)
  (let* ((file-location
          `(:file ,buffer-file-name :line ,(count-lines 1 (point)) :offset ,(- (current-column) (length prefix)))))
    (when (not (spice-member-completion-p prefix))
      (plist-put file-location :prefix prefix))
    (spice-send-command
     "completions"
     file-location
     (lambda (response)
       (funcall
        cb
        (when (spice-response-success-p response)
          (spice-annotate-completions (plist-get response :body) prefix file-location)))))))

(defun spice-command:completion-entry-details (name)
  (let ((arguments (-concat (get-text-property 0 'file-location name)
                            `(:entryNames (,name)))))
    (-when-let (response (spice-send-command-sync "completionEntryDetails" arguments))
      (when (spice-response-success-p response)
        (-when-let (detail (car (plist-get response :body)))
          (mapconcat
           'identity
           (-map (lambda (part) (plist-get part :text)) (plist-get detail :displayParts))
           ""))))))

;;;###autoload
(defun company-spice (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-spice))
    (prefix (and
             (derived-mode-p 'typescript-mode)
             (spice-current-server)
             (not (company-in-string-or-comment))
             (or (spice-completion-prefix) 'stop)))
    (candidates (cons :async
                      (lambda (cb)
                        (spice-command:completions arg cb))))
    (sorted t)
    (meta (spice-command:completion-entry-details arg))))

(defvar spice-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'spice-command:definition)
    map))

(defun spice-setup ()
  (interactive)
  (spice-start-server-if-required)
  (spice-command:configure)
  (spice-command:openfile)
  (spice-mode 1)
  (set (make-local-variable 'eldoc-documentation-function)
       'spice-eldoc-function))

;;;###autoload
(define-minor-mode spice-mode
  "Minor mode for Typescript Interactive Development Environment.

\\{spice-mode-map}"
  :lighter " spice"
  :keymap spice-mode-map
  (if spice-mode
      (progn
        (add-hook 'after-save-hook 'spice-command:reloadfile nil t)
        (add-hook 'after-change-functions 'spice-handle-change nil t)
        (add-hook 'kill-buffer-hook 'spice-remove-tmp-file nil t))
    (remove-hook 'after-save-hook 'spice-command:reloadfile)
    (remove-hook 'after-change-functions 'spice-handle-change)
    (spice-remove-tmp-file)))

(provide 'spice)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; spice.el ends here
