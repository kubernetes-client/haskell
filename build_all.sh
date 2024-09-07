#!/usr/bin/env bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTDIR"

# This script builds kubernetes-client + kubernetes-client-core for
# all Kubernetes versions.

nix run .#set-stack-version 1.20 stack.yaml && stack build
nix run .#set-stack-version 1.21 stack.yaml && stack build
nix run .#set-stack-version 1.22 stack.yaml && stack build
nix run .#set-stack-version 1.23 stack.yaml && stack build
nix run .#set-stack-version 1.24 stack.yaml && stack build
nix run .#set-stack-version 1.25 stack.yaml && stack build
nix run .#set-stack-version 1.26 stack.yaml && stack build
nix run .#set-stack-version 1.27 stack.yaml && stack build
nix run .#set-stack-version 1.28 stack.yaml && stack build
nix run .#set-stack-version 1.29 stack.yaml && stack build
nix run .#set-stack-version 1.30 stack.yaml && stack build
