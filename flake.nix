{
  inputs = {
    nixpkgs.url =
      "https://github.com/NixOS/nixpkgs/archive/32e940c7c420600ef0d1ef396dc63b04ee9cad37.tar.gz";
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
        packages = { tete = tete; };

        defaultPackage =
          pkgs.haskell.lib.justStaticExecutables self.packages.${system}.tete;

        devShells = {
          default = import ./. {
            inherit pkgs;
            isShell = true;
          };
        };
      });
}
