EMACS=$(shell which emacs) -Q -batch -L .
WORKDIR=/tmp/emacs_tide
export HOME := $(WORKDIR)

define ESCRIPT
(with-temp-buffer
  (require 'pp)
  (require 'subr-x)
  (insert-file-contents "tide.el")
  (while
      (ignore-errors
        (let ((sexp (read (current-buffer))))
          (when sexp
            (when (eq (car sexp) 'defcustom)
              (unless (cadr (cddr sexp))
                (princ (format "Documentation missing for defcustom %S\n" (cadr sexp)))
                (kill-emacs 1))
              (princ (format "##### %s `%s`\n\n%s\n\n" (cadr sexp) (string-trim (pp-to-string (car (cddr sexp)))) (replace-regexp-in-string "`\\([^ ]*\\)'" "`\\1`" (cadr (cddr sexp))))))
            t)))))
endef
export ESCRIPT

setup:
	cd .. && git clone git@github.com:Microsoft/TypeScript.git

update-tsserver:
	cd ../TypeScript && npm install && gulp clean && gulp local
	cd ../TypeScript/built/local && cp lib.*.d.ts tsserver.js typingsInstaller.js ../../../tide/tsserver
	cd ../TypeScript && git reset --hard HEAD

cask:
	cask build

test: cask
	mkdir $(WORKDIR) || echo Already has workdir.
	+ $(EMACS) -l tide-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -rf *.elc
	rm -rf $(WORKDIR)

.PHONY: doc test sandbox
doc:
	cd doc && mermaid -c config.json architecture.mmd -w 940 -p

readme:
	ruby -e 'puts IO.read("README.md").split("### Custom Variables")[0] + "### Custom Variables\n\n" + `emacs --batch --eval "$$ESCRIPT"`' | sponge README.md


sandbox:
	rm -rf sandbox
	mkdir sandbox
	emacs -Q --debug \
	        --eval '(setq user-emacs-directory (file-truename "sandbox"))' \
	        -l package \
	        --eval "(add-to-list 'package-archives '(\"gnu\" . \"http://elpa.gnu.org/packages/\") t)" \
	        --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	        --eval "(package-refresh-contents)" \
	        --eval "(package-initialize)" \
	        --eval "(package-install 'tide)" \
                --eval "(when (eq system-type 'darwin) (setq mac-option-key-is-meta nil mac-command-key-is-meta t mac-command-modifier 'meta mac-option-modifier 'none))"

