
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
(dolist (p '(dash flycheck typescript-mode))
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

(require 'tide)
(require 'dash)

;; actual tests:

(ert-deftest strings-get-normalized ()
  "Tests that incoming strings (like in codefixes) get normalized properly."

  (should (equal "this\nis\nfour\nlines"
                 (tide-normalize-lineshift "this\nis\r\nfour\nlines"))))

(ert-deftest member-completetions-get-filtered ()
  "Tests that completions get filtered by kind."
  (let ((mock-completions
         '(
           (:name "DOMError" :kind "interface" :kindModifiers "declare")
           (:name "data" :kind "property" :kindModifiers)
           (:name "declare" :kind "keyword" :kindModifiers)
           (:name "decode" :kind "method" :kindModifiers "declare")
           (:name "deleteText" :kind "local function" :kindModifiers)))
        (filtered-completions
         '((:name "data" :kind "property" :kindModifiers)
           (:name "decode" :kind "method" :kindModifiers "declare"))))
    (should (-same-items?
             (-filter
              (lambda (completion)
                (tide-kind-member-p (plist-get completion :kind)))
              mock-completions)
             filtered-completions))))

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

(provide 'tide-tests)

;;; tide-tests.el ends here
