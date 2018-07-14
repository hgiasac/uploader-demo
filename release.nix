{ compiler ? "ghc822" }:

let
  config = {

  packageOverrides = pkgs: rec {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {

        "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              fileapiOld =
                haskellPackagesNew.callPackage ./default.nix { };

              fileapiNoDoc = pkgs.haskell.lib.dontCheck (
                pkgs.haskell.lib.dontHaddock fileapiOld
              );
              fileapi1 = fileapiNoDoc.overrideDerivation (oldAttrs: {

                  postInstall = ''
                    echo "Copy Backup SQL file"
                    cp -r migration $out
                  '';
                });

            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
{
  fileapi = pkgs.haskell.packages.${compiler}.fileapi1;
}
