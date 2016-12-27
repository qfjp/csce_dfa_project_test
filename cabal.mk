CABALVER := $(shell cabal --numeric-version)

cabal-all : cabal-init
	cabal build
	cabal copy

cabal-test : cabal-init cabal-test-init cabal-all
	cabal test
	cabal check

cabal-clean :
	cabal clean

cabal-init : isDFA
	cabal install --only-dependencies --enable-tests --enable-benchmarks --force-reinstalls --reorder-goals --max-backjumps=-1
	cabal configure

cabal-test-init:
	cabal configure --enable-tests --enable-benchmarks

.PHONY : cabal-all cabal-clean cabal-init cabal-test cabal-test-init
