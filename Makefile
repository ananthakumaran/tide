EMACS=$(shell which emacs) -Q -batch -L .
WORKDIR=/tmp/emacs_tide
export HOME := $(WORKDIR)

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

.PHONY: doc
doc:
	cd doc && mermaid -c config.json architecture.mmd -w 940 -p

# end
