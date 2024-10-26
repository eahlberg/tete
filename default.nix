{ pkgs, isShell }:
let
  hsPkgs = pkgs.haskellPackages;
  hsLib = pkgs.haskell.lib;
  devTools = [ hsPkgs.cabal-install pkgs.dbmate ];
in hsPkgs.developPackage {
  root = ./.;
  withHoogle = false;
  returnShellEnv = isShell;
  modifier = drv:
    if isShell then pkgs.haskell.lib.addBuildTools drv devTools else drv;
}
