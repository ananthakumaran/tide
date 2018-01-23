EMACS=$(shell which emacs) -Q -batch -L .
WORKDIR=/tmp/emacs_tide
export HOME := $(WORKDIR)

define ESCRIPT
(with-temp-buffer
  (require 'pp)
  (insert-file-contents "tide.el")
  (while
      (ignore-errors
        (let ((sexp (read (current-buffer))))
          (when sexp
            (when (eq (car sexp) 'defcustom)
              (unless (cadr (cddr sexp))
                (princ (format "Documentation missing for defcustom %S\n" (cadr sexp)))
                (kill-emacs 1))
              (princ (format "**%s** `%s`\n\n%s\n\n" (cadr sexp) (pp-to-string (car (cddr sexp))) (cadr (cddr sexp)))))
            t)))))
endef
export ESCRIPT

setup:
	cd .. && git clone git@github.com:Microsoft/TypeScript.git

update-tsserver:
	cd ../TypeScript && jake clean && jake local
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

.PHONY: doc test
doc:
	cd doc && mermaid -c config.json architecture.mmd -w 940 -p

readme:
	ruby -e 'puts IO.read("README.md").split("### Custom Variables")[0] + "### Custom Variables\n\n" + `emacs --batch --eval "$$ESCRIPT"`' | sponge README.md

