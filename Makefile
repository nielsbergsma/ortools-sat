documentation:
	cabal haddock --haddock-html
	rm -rf docs/
	mv dist-newstyle/build/aarch64-osx/ghc-9.10.1/ortools-sat-0.1.0.0/doc/html/ortools-sat/ docs