
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


(defun tide-plist-map (fn plist)
  (-map (-lambda ((key value)) (funcall fn key value)) (-partition 2 plist)))

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

(ert-deftest completions-get-sorted ()
  "Tests that completion candidates get sorted by kind."
  (let ((mock-completions
         '(
           (:name "DOMError" :kind "interface" :kindModifiers "declare")
           (:name "data" :kind "var" :kindModifiers)
           (:name "debugger" :kind "keyword" :kindModifiers)
           (:name "declare" :kind "keyword" :kindModifiers)
           (:name "decodeURI" :kind "function" :kindModifiers "declare")
           (:name "decodeURIComponent" :kind "function" :kindModifiers "declare")
           (:name "deleteText" :kind "local function" :kindModifiers)
           (:name "dimensions" :kind "parameter" :kindModifiers)
           (:name "document" :kind "var" :kindModifiers "declare")))
        (sorted-completions
         '(
           (:name "dimensions" :kind "parameter" :kindModifiers)
           (:name "data" :kind "var" :kindModifiers)
           (:name "deleteText" :kind "local function" :kindModifiers)
           (:name "debugger" :kind "keyword" :kindModifiers)
           (:name "declare" :kind "keyword" :kindModifiers)
           (:name "document" :kind "var" :kindModifiers "declare")
           (:name "decodeURI" :kind "function" :kindModifiers "declare")
           (:name "decodeURIComponent" :kind "function" :kindModifiers "declare")
           (:name "DOMError" :kind "interface" :kindModifiers "declare"))
         ))
    (should (-same-items?
             (-sort 'tide-compare-completions mock-completions)
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
