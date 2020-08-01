;;; tide-tests --- This file contains automated tests for tide.el

;;; Commentary:
;; Run tests using (ert-run-tests-interactively t).

;;; Code:


;; Test setuup:

(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))

;; tide depends on typescript-mode
(dolist (p '(dash s flycheck typescript-mode))
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

(require 'tide)
(require 'dash)

(defun tide-plist-equal (a b)
  (and (listp a) (listp b)
       (= (length a) (length b))
       (-all? #'identity
              (tide-plist-map
               (lambda (key value)
                 (if (listp value)
                     (tide-plist-equal value (plist-get a key))
                   (equal (plist-get a key) value)))
               b))))

;; actual tests:

(ert-deftest tide-compose-comparators-tests ()
  "Tests that confirm the implementation of `tide-compose-comparators'."
  (let ((<< (tide-compose-comparators (lambda (x y) (< (car  x) (car  y)))
                                      (lambda (x y) (< (cadr x) (cadr y))))))
    (should (equal nil (funcall << '(0 1) '(0 1))))
    (should (equal nil (funcall << '(0 1) '(0 0))))
    (should (equal t   (funcall << '(0 0) '(0 1))))
    (should (equal nil (funcall << '(1 0) '(0 0))))
    (should (equal t   (funcall << '(0 0) '(1 0))))
    (should (equal nil (funcall << '(1 0) '(0 1))))
    (should (equal t   (funcall << '(0 1) '(1 0))))))

(ert-deftest strings-get-normalized ()
  "Tests that incoming strings (like in codefixes) get normalized properly."

  (should (equal "this\nis\nfour\nlines"
                 (tide-normalize-lineshift "this\nis\r\nfour\nlines"))))

(ert-deftest completions-get-sorted-by-sort-text ()
  "Tests that completion candidates get sorted by `sortText' property."
  (let ((mock-completions
         '((:name "DOMError" :kind "interface" :kindModifiers "declare" :sortText "0")
           (:name "action" :kind "warning" :kindModifiers "" :sortText "1")
           (:name "data" :kind "var" :kindModifiers "" :sortText "0")
           (:name "debugger" :kind "keyword" :kindModifiers "" :sortText "0")
           (:name "declare" :kind "keyword" :kindModifiers "" :sortText "0")
           (:name "decodeURI" :kind "function" :kindModifiers "declare" :sortText "0")
           (:name "decodeURIComponent" :kind "function" :kindModifiers "declare" :sortText "0")
           (:name "deleteText" :kind "local function" :kindModifiers "" :sortText "0")
           (:name "deque" :kind "warning" :kindModifiers "" :sortText "1")
           (:name "dimensions" :kind "parameter" :kindModifiers "" :sortText "0")
           (:name "document" :kind "var" :kindModifiers "declare" :sortText "0")))
        (sorted-completions
         '((:name "DOMError" :kind "interface" :kindModifiers "declare" :sortText "0")
           (:name "data" :kind "var" :kindModifiers "" :sortText "0")
           (:name "debugger" :kind "keyword" :kindModifiers "" :sortText "0")
           (:name "declare" :kind "keyword" :kindModifiers "" :sortText "0")
           (:name "decodeURI" :kind "function" :kindModifiers "declare" :sortText "0")
           (:name "decodeURIComponent" :kind "function" :kindModifiers "declare" :sortText "0")
           (:name "deleteText" :kind "local function" :kindModifiers "" :sortText "0")
           (:name "dimensions" :kind "parameter" :kindModifiers "" :sortText "0")
           (:name "document" :kind "var" :kindModifiers "declare" :sortText "0")
           (:name "action" :kind "warning" :kindModifiers "" :sortText "1")
           (:name "deque" :kind "warning" :kindModifiers "" :sortText "1"))))
    (should (equal
             (-sort 'tide-compare-completions-basic mock-completions)
             sorted-completions))))

(ert-deftest completions-get-sorted-by-kind ()
  "Tests that completion candidates get sorted by kind."
  (let ((mock-completions
         '((:name "DOMError" :kind "interface" :kindModifiers "declare" :sortText "0")
           (:name "action" :kind "warning" :kindModifiers "" :sortText "1")
           (:name "data" :kind "var" :kindModifiers "" :sortText "0")
           (:name "debugger" :kind "keyword" :kindModifiers "" :sortText "0")
           (:name "declare" :kind "keyword" :kindModifiers "" :sortText "0")
           (:name "decodeURI" :kind "function" :kindModifiers "declare" :sortText "0")
           (:name "decodeURIComponent" :kind "function" :kindModifiers "declare" :sortText "0")
           (:name "deleteText" :kind "local function" :kindModifiers "" :sortText "0")
           (:name "deque" :kind "warning" :kindModifiers "" :sortText "1")
           (:name "dimensions" :kind "parameter" :kindModifiers "" :sortText "0")
           (:name "document" :kind "var" :kindModifiers "declare" :sortText "0")))
        (sorted-completions
         '((:name "dimensions" :kind "parameter" :kindModifiers "" :sortText "0")
           (:name "deleteText" :kind "local function" :kindModifiers "" :sortText "0")
           (:name "data" :kind "var" :kindModifiers "" :sortText "0")
           (:name "action" :kind "warning" :kindModifiers "" :sortText "1")
           (:name "debugger" :kind "keyword" :kindModifiers "" :sortText "0")
           (:name "declare" :kind "keyword" :kindModifiers "" :sortText "0")
           (:name "deque" :kind "warning" :kindModifiers "" :sortText "1")
           (:name "document" :kind "var" :kindModifiers "declare" :sortText "0")
           (:name "decodeURI" :kind "function" :kindModifiers "declare" :sortText "0")
           (:name "decodeURIComponent" :kind "function" :kindModifiers "declare" :sortText "0")
           (:name "DOMError" :kind "interface" :kindModifiers "declare" :sortText "0"))))
    (should (equal
             (-sort 'tide-compare-completions-by-kind mock-completions)
             sorted-completions))))

(ert-deftest tide-plist-equal ()
  (should (tide-plist-equal '() '()))
  (should-not (tide-plist-equal '(:a 1) '()))
  (should (tide-plist-equal '(:a 1) '(:a 1)))
  (should (tide-plist-equal '(:a 1 :b (:nest 1)) '(:a 1 :b (:nest 1))))
  (should (tide-plist-equal '(:a 1 :b (:nest 1 :nest2 2)) '(:a 1 :b (:nest2 2 :nest 1))))
  (should-not (tide-plist-equal '(:a 1 :b (:nest 1)) '(:a 1 :b (:nest 2)))))

;; Adapted from jdee-mode's test suite.
(defmacro mode-with-temp-buffer (content &rest body)
  "Fill a temporary buffer with `CONTENT', turn on `tide-mode' and eval `BODY' in it."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (tide-mode)
     (goto-char (point-min))
     ;; We need this so that tests that simulate user actions operate on the right buffer.
     (switch-to-buffer (current-buffer))
     ,@body))

(defmacro setup-with-temp-buffer (content &rest body)
  "Fill a temporary buffer with `CONTENT', invoke `tide-setup' and eval `BODY' in it."
  (declare (debug t)
           (indent 1))
  (let ((server (make-symbol "server")))
    `(with-temp-buffer
       (insert ,content)
       (tide-setup)
       (goto-char (point-min))
       ;; We need this so that tests that simulate user actions operate on the right buffer.
       (switch-to-buffer (current-buffer))
       (save-current-buffer ,@body)
       (tide-kill-server))))

(defmacro wait-for (&rest body)
  "Wait until BODY executes without error.  There's an arbitrary 5 second
timeout.  If BODY does not execute without error before the timeout, that's
a test failure."
  (declare (debug t)
           (indent 1))
  `(with-timeout (5 (ert-fail "Timed out"))
     ;; If an error occurs in BODY, ignore-error will return nil.
     (while (not (ignore-errors ,@body t))
       ;; We need to let emacs process tsserver output.
       (accept-process-output nil 0.1))))

(cl-defstruct spy
  "A spy is used to keep track of calls on an actual function."
  called
  (call-count 0))

(defun reset-spy (spy)
  "Reset `SPY' to the same default state a new spy gets."
  (setf (spy-called spy) nil
        (spy-call-count spy) 0))


(defmacro with-spy (name function-symbol &rest body)
  "Execute `BODY' with `NAME' bound to a spy that spies on `FUNCTION-SYMBOL'."
  (declare (debug (symbolp symbolp body))
           (indent 2))
  ;; Avoid polluting the scope of the code using this macro.
  (let ((orig (make-symbol "orig"))
        (spy (make-symbol "spy"))
        (fn-sym (make-symbol "fn-sym")))
    `(let* (,orig
            ;; Internal reference to the spy so we don't have to evaluate
            ;; multiple times.
            (,spy (make-spy))
            (,name ,spy) ;; We bind the spy to the name specified by the user.
            (,fn-sym ',function-symbol))
       (cl-letf (((symbol-function ',orig) (symbol-function ,fn-sym))
                 ((symbol-function ,fn-sym)
                  (lambda (&rest args)
                    (apply ',orig args)
                    (setf (spy-called ,spy) t)
                    (cl-incf (spy-call-count ,spy)))))
         ,@body))))

(defmacro with-open-file (file-path &rest body)
  "Open the file `FILE-PATH' and execute `BODY' in the buffer created
for the file. The tide server and buffer are killed after the execution
of `BODY'."
  (declare (debug t)
           (indent 1))
  (let ((buffer-sym (make-symbol "buffer")))
    `(let* ((,buffer-sym (find-file ,file-path)))
       (unwind-protect
           (progn
             (tide-setup)
             ,@body)
         (tide-kill-server)
         (kill-buffer ,buffer-sym)))))

(defun test-tide-add-tslint-disable-next-line (mock initial goto expected)
  (mode-with-temp-buffer
   initial
   (search-forward goto nil t)
   (goto-char (match-beginning 0))
   (cl-letf (((symbol-function 'tide-get-flycheck-errors-ids-at-point)
              (lambda () mock)))
     (tide-add-tslint-disable-next-line)
     (should (string= (buffer-string) expected)))))

(ert-deftest tide-add-tslint-disable-next-line/no-errors ()
  "If there are no flycheck errors at point, it should be a no-op."
  (let ((initial "// tslint:disable-next-line:previous\nconst x = 1;\nconst y = 2;\n"))
    (test-tide-add-tslint-disable-next-line
     ()
     initial
     "const y"
     initial)))

;; The first line of the buffer consitutes an edge condition. An
;; earlier version of the function failed in this case.
(ert-deftest tide-add-tslint-disable-next-line/first-line ()
  "Create a new tslint flag if needed. (First line of buffer)"
  (let ((initial "const x = 1;\nconst y = 2;\n"))
    (test-tide-add-tslint-disable-next-line
     '("err1" "err2")
     initial
     "const x"
     (concat "// tslint:disable-next-line:err1 err2\n" initial))))

(ert-deftest tide-add-tslint-disable-next-line/subsequent-lines ()
  "Create a new tslint flag if needed. (Subsequent lines of buffer)"
  (test-tide-add-tslint-disable-next-line
   '("err1" "err2")
   "const x = 1;\nconst y = 2;\n"
   "const y"
   "const x = 1;\n// tslint:disable-next-line:err1 err2\nconst y = 2;\n"))

(ert-deftest tide-add-tslint-disable-next-line/adds ()
  "If there was already a disable-next-line flag, add to it."
  (test-tide-add-tslint-disable-next-line
   '("err1" "err2")
   "// tslint:disable-next-line:previous\nconst x = 1;\nconst y = 2;\n"
   "const x"
   "// tslint:disable-next-line:previous err1 err2\nconst x = 1;\nconst y = 2;\n"))

(ert-deftest tide-setup ()
  "Test that tide-setup can be invoked without errors."
  (setup-with-temp-buffer
   "const foo = 1;"))

(ert-deftest tide-setup/warns-about-old-emacs ()
  "Test that tide-setup warns about old emacs."
  (let* ((emacs-version "1")
         (display-warning)
         (seen-type)
         (seen-message)
         (seen-level))
    (fset 'display-warning
          (lambda (type message level)
            (setq seen-type type)
            (setq seen-message message)
            (setq seen-level level)))
    (setup-with-temp-buffer
     "const foo = 1;")
    (should (equal seen-type 'tide))
    (should (equal seen-message "Tide requires Emacs >= 25.1, you are using 1."))
    (should (equal seen-level :error))))

(ert-deftest tide-setup/immediate-server-start ()
  "Test that tide-setup with immediate start method starts a
server and configures the buffer."
  ;; 'immediate is the default but the default could change some day
  (let ((tide-tsserver-start-method 'immediate))
    (with-spy configure-spy tide-command:configure
      (with-open-file "test/trivial.ts"
        (should (not (eq (tide-current-server) nil)))
        (should (spy-called configure-spy))
        (reset-spy configure-spy)
        ;; Opening a second file in the same project follows a different code path.
        (with-open-file "test/trivial2.ts"
          (should (not (eq (tide-current-server) nil)))
          (should (spy-called configure-spy)))))))

(ert-deftest tide-setup/manual-server-start ()
  "Test that tide-setup with manual start method does not start a server."
  (let ((tide-tsserver-start-method 'manual))
    (with-spy configure-spy tide-command:configure
      (with-open-file "test/trivial.ts"
        (should (eq (tide-current-server) nil))
        (should (not (spy-called configure-spy)))
        ;; We test with a 2nd file for parallelism with the other similar tests.
        (with-open-file "test/trivial2.ts"
          (should (eq (tide-current-server) nil))
          (should (not (spy-called configure-spy))))))))

(ert-deftest tide-setup/manual-server-start-with-start-between-files ()
  "Test that when a server is started after an initial buffer was
opened in manual mode, a 2nd buffer opened in the same project is
automatically configured."
  (let ((tide-tsserver-start-method 'manual))
    (with-spy configure-spy tide-command:configure
      (with-open-file "test/trivial.ts"
        (should (eq (tide-current-server) nil))
        (should (not (spy-called configure-spy)))
        (tide-restart-server)
        (should (spy-called configure-spy))
        (reset-spy configure-spy)
        (with-open-file "test/trivial2.ts"
          (should (not (eq (tide-current-server) nil)))
          (should (spy-called configure-spy)))))))

(ert-deftest tide-setup/manual-server-start-late-start ()
  "Test that when a server is started after multiple buffers were
opened in the same project in manual mode, all buffers are configured."
  (let ((tide-tsserver-start-method 'manual))
    (with-spy configure-spy tide-command:configure
      (with-open-file "test/trivial.ts"
        (should (eq (tide-current-server) nil))
        (should (not (spy-called configure-spy)))
        (with-open-file "test/trivial2.ts"
          (should (eq (tide-current-server) nil))
          (should (not (spy-called configure-spy)))
          (tide-restart-server)
          (should (not (eq (tide-current-server) nil)))
          (should (eq (spy-call-count configure-spy) 2)))))))

(ert-deftest tide-list-servers/smoketest ()
  "Test that tide-list-servers can be invoked."
  (setup-with-temp-buffer
   "const foo = 1;"
   (tide-list-servers)
   (switch-to-buffer "*Tide Server List*")
   (should (string-match-p (concat "tide-.*\\s-+\\(--\\|[0-9]+\\)\\s-+" default-directory)
                           (buffer-substring-no-properties (point-min) (point-max))))))

;; All lines in the Tide Server List buffer match this pattern.
(setq common-server-buffer-pattern "^tide-.*\\s-+\\(--\\|[0-9]+\\)\\s-+")

;; All lines in the Tide Server List buffer match this pattern when the last
;; column shows the project directory (which is the default when the buffer
;; is created).
(setq directory-server-buffer-pattern (concat  common-server-buffer-pattern default-directory "$"))

(setq path-server-buffer-pattern (concat common-server-buffer-pattern "node " default-directory
                                  "tsserver/tsserver.js$"))

(ert-deftest tide-list-servers/cycle-last-column ()
  "Test that we can cycle the last column of the server list."
  (setup-with-temp-buffer
   "const foo = 1;"
   (tide-list-servers)
   (switch-to-buffer "*Tide Server List*")
   (should (string-match-p directory-server-buffer-pattern (buffer-string)))
   (call-interactively (key-binding "/"))
   (should (string-match-p path-server-buffer-pattern (buffer-string)))
   (call-interactively (key-binding "/"))
   (should (string-match-p directory-server-buffer-pattern (buffer-string)))))

(ert-deftest tide-list-servers/kill-server ()
  "Test that we can kill servers from the server list."
  (setup-with-temp-buffer
   "const foo = 1;"
   (tide-list-servers)
   (switch-to-buffer "*Tide Server List*")
   (should (string-match-p directory-server-buffer-pattern (buffer-string)))
   (should (= (hash-table-count tide-servers) 1))
   (call-interactively (key-binding "d"))
   (should (= (hash-table-count tide-servers) 0))
   (should (string= "" (buffer-string)))))

;; All lines in the Tide Server List buffer match this pattern.
(setq test-common-server-buffer-pattern "^test-.*\\s-+\\(--\\|[0-9]+\\)\\s-+")

(ert-deftest tide-list-servers/verify-setup ()
  "Test that hitting enter on the project name verifies the setup."
  ;; We need a file-backed buffer for this. Otherwise, tsserver errors.
  (with-open-file "test/trivial.ts"
    (let* ((buffer-pattern (concat test-common-server-buffer-pattern default-directory "$")))
      (tide-list-servers)
      (switch-to-buffer "*Tide Server List*")
      (should (string-match-p buffer-pattern (buffer-string)))
      (execute-kbd-macro (kbd "<return>"))
      ;; The operation is asynchronous so we have to wait for it.
      (wait-for
       (should (member "*tide-project-info*" (mapcar (function buffer-name) (buffer-list))))))))

(ert-deftest test-tide-hl-identifier/spaces ()
  "Test that `tide-hl-identifier' highlights the identifiers properly in a
file that uses spaces for indentation."
  (let* ((buffer (find-file "test/highlight.ts")))
    (tide-setup)
    (re-search-forward "Fnord")
    (tide-hl-identifier)
    ;; `tide-hl-identifier' is asynchronous so we need to wait until
    ;; it is done.
    (wait-for
     (should (= (length (overlays-in (point-min) (point-max))) 5)))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (should (string= (buffer-substring (overlay-start overlay)
                                         (overlay-end overlay))
                       "Fnord")))
    (delete-process (tide-current-server))
    (kill-buffer buffer)))

(ert-deftest test-tide-hl-identifier/tabs ()
  "Test that `tide-hl-identifier' highlights the identifiers properly in a
file that uses tabs for indentation."
  (let* ((buffer (find-file "test/highlight-tabs.ts")))
    (tide-setup)
    (goto-char (point-max))
    (re-search-backward "a: ")
    (tide-hl-identifier)
    ;; `tide-hl-identifier' is asynchronous so we need to wait until
    ;; it is done.
    (wait-for
     (should (= (length (overlays-in (point-min) (point-max))) 5)))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (should (string= (buffer-substring (overlay-start overlay)
                                         (overlay-end overlay))
                       "a")))
    (delete-process (tide-current-server))
    (kill-buffer buffer)))
(when (>= emacs-major-version 27)
  (ert-deftest test-tide-safe-json-read-file ()
    "Test that `tide-safe-json-read-file' produces the same output for both json.el and the native json parser"
    (let ((native-output
           (let ((tide-native-json-parsing t))
             (tide-safe-json-read-file "test/test.json")))
          (elisp-output
           (let ((tide-native-json-parsing nil))
             (tide-safe-json-read-file "test/test.json"))))
      (should (equal native-output elisp-output)))))

(when (>= emacs-major-version 27)
  (ert-deftest test-tide-safe-json-read-string ()
    "Test that `tide-safe-json-read-string' produces the same output for both native and elisp JSON libraries"
    (let* ((test-json
            (with-temp-buffer
              (insert-file-contents "test/test.json")
              (buffer-string)))
           (native-output
            (let ((tide-native-json-parsing t))
              (tide-safe-json-read-string test-json)))
           (elisp-output
            (let ((tide-native-json-parsing nil))
              (tide-safe-json-read-string test-json))))
      (should (equal native-output elisp-output)))))

(when (>= emacs-major-version 27)
  (ert-deftest test-tide-json-read-object ()
    "Test that `tide-json-read-object' produces the same output for both native and elisp JSON libraries"
    (let* ((native-output
            (with-temp-buffer
              (insert-file-contents "test/test.json")
              (let ((tide-native-json-parsing t))
                (tide-json-read-object))))
           (elisp-output
            (with-temp-buffer
              (insert-file-contents "test/test.json")
              (let ((tide-native-json-parsing nil))
                (tide-json-read-object)))))
      (should (equal native-output elisp-output)))))

(when (>= emacs-major-version 27)
  (ert-deftest test-tide-json-encode ()
    "Test that `tide-json-read-object' produces the same JSON string for both native and elisp libraries"
    (let* ((json-obj `(:number 123
                               :string "string"
                               :true t
                               :false ,json-false
                               :array [(:number 123 :string "string" :true t :false ,json-false)]))
           (native-string (let ((tide-native-json-parsing t))
                            (tide-json-encode json-obj)))
           (elisp-string (let ((tide-native-json-parsing nil))
                           (tide-json-encode json-obj))))
      (should (equal native-string elisp-string))))

  (provide 'tide-tests))

;;; tide-tests.el ends here
