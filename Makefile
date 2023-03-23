doc:
	cd && cabal haddock --haddock-executables

build:
	nix build .#housekeeper

server:
	cd && cabal run

.PHONY: doc build server
