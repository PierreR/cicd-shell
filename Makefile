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

build-docker:
	nix-build release.nix -A docker

load-docker: build-docker
	docker load < result

run-docker:
	./docker/run.sh $(CMD)

clean:
	@nix-shell --run "cabal clean"
	rm -f share/doc/*.*
