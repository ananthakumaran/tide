setup:
	cd .. && git clone git@github.com:Microsoft/TypeScript.git

update-tsserver:
	cd ../TypeScript && git apply ../tide/geterr.patch
	cd ../TypeScript && jake clean && jake local
	cd ../TypeScript/built/local && cp lib.core.d.ts lib.core.es6.d.ts lib.d.ts lib.dom.d.ts lib.es6.d.ts lib.scriptHost.d.ts lib.webworker.d.ts tsserver.js ../../../tide/tsserver
	cd ../TypeScript && git reset --hard HEAD

