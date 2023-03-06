# Kubernetes Haskell Client

[![Cabal](https://github.com/kubernetes-client/haskell/actions/workflows/cabal.yml/badge.svg)](https://github.com/kubernetes-client/haskell/actions/workflows/cabal.yml)
[![Stack](https://github.com/kubernetes-client/haskell/actions/workflows/stack.yml/badge.svg)](https://github.com/kubernetes-client/haskell/actions/workflows/stack.yml)
[![Build Status](https://travis-ci.org/kubernetes-client/haskell.svg?branch=master)](https://travis-ci.org/kubernetes-client/haskell)

Haskell client for the [kubernetes](http://kubernetes.io/) API.

## Contribute

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for instructions on how to contribute.

# Development

## Update client

to update the client clone the `gen` repo and run this command at the root of the client repo:

```bash
${GEN_REPO_BASE}/openapi/haskell.sh kubernetes settings
```
