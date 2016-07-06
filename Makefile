setup:
	cd .. && git clone git@github.com:Microsoft/TypeScript.git

update-tsserver:
	cd ../TypeScript && jake clean && jake local
	cd ../TypeScript/built/local && cp lib.es5.d.ts lib.es6.d.ts lib.es2015.d.ts lib.es2015.core.d.ts lib.es2015.collection.d.ts lib.es2015.generator.d.ts lib.es2015.iterable.d.ts lib.es2015.promise.d.ts lib.es2015.proxy.d.ts lib.es2015.reflect.d.ts lib.es2015.symbol.d.ts lib.es2015.symbol.wellknown.d.ts lib.d.ts lib.dom.d.ts lib.scripthost.d.ts lib.webworker.d.ts tsserver.js ../../../tide/tsserver
	cd ../TypeScript && git reset --hard HEAD

