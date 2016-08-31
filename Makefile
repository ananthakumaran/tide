setup:
	cd .. && git clone git@github.com:Microsoft/TypeScript.git

update-tsserver:
	cd ../TypeScript && jake clean && jake local
	cd ../TypeScript/built/local && cp lib.*.d.ts tsserver.js ../../../tide/tsserver
	cd ../TypeScript && git reset --hard HEAD

