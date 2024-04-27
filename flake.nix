{
  description = "Flake for TyTTP";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem
    (
      system: let
        stable = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = stable.mkShell {
          packages = with stable; [
            nodejs_18
            chez

            stdenv
            gmp
          ];

          shellHook = ''
            export PACK_DIR=`pwd`/.pack
            export PATH=$PACK_DIR/bin:$PATH
            if [ ! -d $PACK_DIR ]; then
              sh -c "$(${stable.curl}/bin/curl -fsSL https://raw.githubusercontent.com/stefan-hoeck/idris2-pack/main/install.bash)"
            fi
            [ ! -e $PACK_DIR/bin/idris2-lsp ] && pack --no-prompt install-app idris2-lsp
          '';
        };
      }
    );
}
