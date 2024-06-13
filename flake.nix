{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gen = {
    url = "github:kubernetes-client/gen";
    flake = false;
  };
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=release-24.05";

  outputs = { self, flake-utils, gen, gitignore, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };

      in {
        packages = rec {
          generate = pkgs.writeShellScriptBin "generate-kubernetes-client-core.sh" ''
            export KUBERNETES_VERSION="$1"
            PACKAGE_VERSION="$2"
            out="kubernetes-$KUBERNETES_VERSION"

            # Generate
            ${pkgs.bash}/bin/bash "${gen}/openapi/haskell.sh" "$out" settings

            # Fill in the package version
            ${pkgs.gnused}/bin/sed -i "s/^version:\s*\(.*\)/version:        $PACKAGE_VERSION/" "$out/kubernetes-client-core.cabal"

            # Fix a bound
            ${pkgs.gnused}/bin/sed -i 's/\(http-api-data >= 0.3.4 &&\) <0.6/\1 <0.7/' "$out/kubernetes-client-core.cabal"
          '';

          set-stack-version = pkgs.writeShellScriptBin "build-kubernetes-client.sh" ''
            export KUBERNETES_VERSION="$1"
            STACK_YAML="$2"

            ${pkgs.gnused}/bin/sed -i "s/^- kubernetes-\(1\.\)[0-9]\+/- kubernetes-$KUBERNETES_VERSION/" "$STACK_YAML"
          '';

          set-cabal-version = pkgs.writeShellScriptBin "build-kubernetes-client.sh" ''
            export KUBERNETES_VERSION="$1"
            CABAL_PROJECT="$2"

            ${pkgs.gnused}/bin/sed -i "s/^  kubernetes-\(1\.\)[0-9]\+/  kubernetes-$KUBERNETES_VERSION/" "$CABAL_PROJECT"
          '';
        };
      });
}
