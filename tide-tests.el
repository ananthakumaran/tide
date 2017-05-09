
;;; tide-tests --- This file contains automated tests for tide.el

;;; Commentary:
;; Run tests using (ert-run-tests-interactively t).

;;; Code:

(require 'ert)
;;(require 'tide)

(ert-deftest ci-test ()
  "Test which checks that ERT-tests are run on CI-builds.")


(provide 'tide-tests)

;;; tide-tests.el ends here
