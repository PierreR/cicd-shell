.PHONY: compile clean local configure clean

compile:
	@cabal build

configure:
	@nix-shell --run "cabal configure"

local:
	@nix-shell --run "cabal install"

doc: share/doc/cicd-shell.html share/doc/cicd-shell.pdf

share/doc/cicd-shell.html: README.adoc
	@nix-shell -p asciidoctor --command "asciidoctor $< -o $@"

share/doc/cicd-shell.pdf: README.adoc
	@nix-shell -p asciidoctor --command "asciidoctor -r asciidoctor-pdf -b pdf $< -o $@"

clean:
	@nix-shell --run "cabal clean"
	rm -f share/doc/*.*
