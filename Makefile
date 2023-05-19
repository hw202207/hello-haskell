clean:
	cabal clean

build: clean
	cabal build -j

build2: clean
	cabal build -j --ghc-options="-fwrite-if-simplified-core"
