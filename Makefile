.PHONY: build-docker docker run-docker push-docker clean

build-docker:
	nix-build release.nix -A docker

docker: build-docker
	docker load < result

run-docker:
	./docker/run.sh $(CMD)

push-docker:
	docker push cicd-docker.repository.irisnet.be/cicd-shell

clean:
	@nix-shell --run "cabal clean"
