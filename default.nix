{ mkDerivation, aeson, array, base, bytestring, call-stack
, containers, data-has, directory, distributive, filepath, hspec
, http-types, kan-extensions, mtl, resource-pool, scientific
, scotty, semigroups, split, sqlite-simple, stdenv
, template-haskell, text, time, transformers, utf8-string, vector
, wai, wai-cors, wai-extra, wai-middleware-static, warp
}:
mkDerivation {
  pname = "file-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson array base bytestring call-stack containers data-has
    directory distributive filepath hspec http-types kan-extensions mtl
    resource-pool scientific scotty semigroups split sqlite-simple
    template-haskell text time transformers utf8-string vector wai
    wai-cors wai-extra wai-middleware-static warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec mtl sqlite-simple ];
  license = stdenv.lib.licenses.bsd3;


  postInstall = ''
    echo "Copy Backup SQL file"
    cp -r migration $out

    mkdir -p $out/share/nix/fileapi
    cp release-configuration.nix $out/share/nix/fileapi/

  '';
}
