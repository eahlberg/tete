{
  inputs = {
    nixpkgs.url =
      # Sat Nov 23 05:11:19 PM CET 2024
      "https://github.com/NixOS/nixpkgs/archive/057f63b6dc1a2c67301286152eb5af20747a9cb4.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.bash-prompt = "[dev] ";

  outputs = { self, nixpkgs, flake-utils }:

    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        tete = import ./. {
          inherit pkgs;
          isShell = false;
        };
      in {
        packages = {
          tete = tete;
          default =
            pkgs.haskell.lib.justStaticExecutables self.packages.${system}.tete;
        };

        devShells = {
          default = import ./. {
            inherit pkgs;
            isShell = true;
          };
        };
      });
}
