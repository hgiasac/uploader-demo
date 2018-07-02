{ compiler ? "ghc822" }:

let
  config = {

  packageOverrides = pkgs: rec {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${compiler}" = pkgs.haskell.packages."${compiler}".override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {

            fileapi =
              haskellPackagesNew.callPackage ./default.nix { };


            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
{
  fileapi = pkgs.haskell.packages.${compiler}.fileapi;
}
