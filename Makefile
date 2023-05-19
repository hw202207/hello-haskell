clean:
	cabal clean

build: clean
	cabal build

build2: clean
	cabal build --ghc-options="-fwrite-if-simplified-core"
