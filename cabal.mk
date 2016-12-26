cabal-all : cabal-init
	cabal build

cabal-test : isDFA cabal-init
	cabal test

cabal-clean : cabal-init
	cabal clean

cabal-init :
	cabal sandbox init

.PHONY : cabal-all cabal-clean cabal-init cabal-test
