doc:
	cabal haddock --haddock-executables

build:
	cabal build

server:
	cabal run

.PHONY: doc build server
