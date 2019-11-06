# This is currently a deployment shell rather than a development shell
# In that regard, this shell gives you an environment to test the cicd-shell (not to develop it)
(import ./release.nix {}).shell
