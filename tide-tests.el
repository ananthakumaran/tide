
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
(dolist (p '(typescript-mode))
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

(require 'tide)

;; actual tests:

(ert-deftest strings-get-normalized ()
  "Tests that incoming strings (like in codefixes) get normalized properly."

  (should (equal "this\nis\nfour\nlines"
                 (tide-normalize-lineshift "this\nis\r\nfour\nlines"))))

(provide 'tide-tests)

;;; tide-tests.el ends here
