.PHONY: compile clean local configure

compile:
	@cabal build

configure:
	@nix-shell --run "cabal configure"

local:
	@cabal install

clean:
	@nix-shell --run "cabal clean"
