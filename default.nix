let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
  addBuildTools = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install
            haskell-language-server
          ]);
  addExternDeps = drv: pkgs.haskell.lib.addExtraLibraries drv (
    [ pkgs.SDL2 ]);
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv: (addBuildTools (addExternDeps drv));
}
