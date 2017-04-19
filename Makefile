.PHONY: compile clean local configure clean

compile:
	@cabal build

configure:
	@nix-shell --run "cabal configure"

local:
	@cabal install

doc: doc/cicd-shell.html doc/cicd-shell.pdf

doc/cicd-shell.html: README.adoc
	@nix-shell -p asciidoctor --command "asciidoctor $< -o $@"

doc/cicd-shell.pdf: README.adoc
	@nix-shell -p asciidoctor --command "asciidoctor -r asciidoctor-pdf -b pdf $< -o $@"

clean:
	@nix-shell --run "cabal clean"
	rm -f doc/*.*
