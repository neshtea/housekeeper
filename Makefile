doc:
	cabal haddock --haddock-executables

build:
	nix build .#housekeeper

server:
	cabal run

.PHONY: doc build server
