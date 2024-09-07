#!/usr/bin/env bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTDIR"

# The first argument is the Kubernetes release to use.
# The second is the package version to place in ./kubernetes-<K8S version>/kubernetes-client-core.cabal.
# The idea is to use the patch number for our own purposes, incrementing when we need to update
# the kubernetes-client-core library.

nix run .#generate -- 1.20 1.20.0
nix run .#generate -- 1.21 1.21.0
nix run .#generate -- 1.22 1.22.0
nix run .#generate -- 1.23 1.23.0
nix run .#generate -- 1.24 1.24.0
nix run .#generate -- 1.25 1.25.0
nix run .#generate -- 1.26 1.26.0
nix run .#generate -- 1.27 1.27.0
nix run .#generate -- 1.28 1.28.0
nix run .#generate -- 1.29 1.29.0
nix run .#generate -- 1.30 1.30.0
