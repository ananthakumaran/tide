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

(provide 'tide-tests)

;;; tide-tests.el ends here
