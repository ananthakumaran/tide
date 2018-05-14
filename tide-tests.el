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
(require 'cl)

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

(ert-deftest load-tsconfig ()
  (should (tide-plist-equal '(:compilerOptions (:target "ES7" :sourceMap t) :extends "./base.json" :compileOnSave t)
                 (tide-load-tsconfig "test/tsconfig.json" '())))

  (should (tide-plist-equal '(:compilerOptions (:target "ES7" :sourceMap t) :extends "./base" :compileOnSave t)
                            (tide-load-tsconfig "test/tsconfig-no-extension.json" '())))

  (should (tide-plist-equal '(:compileOnSave t :compilerOptions (:target "ES6" :sourceMap t))
                 (tide-load-tsconfig "test/base.json" '())))

  (should-error (tide-load-tsconfig "test/loop.json" '()))

  (should-error (tide-load-tsconfig "test/notfound.json" '())))

;; Adapted from jdee-mode's test suite.
(defmacro test-with-temp-buffer (content &rest body)
  "Fill a temporary buffer with `CONTENT' and eval `BODY' in it."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (tide-mode)
     (goto-char (point-min))
     ;; We need this so that tests that simulate user actions operate on the right buffer.
     (switch-to-buffer (current-buffer))
     ,@body))

(defun test-tide-add-tslint-disable-next-line (mock initial goto expected)
  (test-with-temp-buffer
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

(provide 'tide-tests)

;;; tide-tests.el ends here
