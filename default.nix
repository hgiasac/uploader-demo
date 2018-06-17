{ mkDerivation, aeson, array, base, bytestring, call-stack
, classy-prelude, containers, data-has, digestive-functors
, digestive-functors-aeson, directory, distributive, filepath
, hspec, http-types, kan-extensions, lens, mtl, resource-pool
, scientific, scotty, semigroups, split, sqlite-simple, stdenv
, template-haskell, text, time, transformers, utf8-string, vector
, wai, wai-cors, wai-extra, wai-middleware-static, warp
}:
mkDerivation {
  pname = "file-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bytestring call-stack classy-prelude containers
    data-has digestive-functors digestive-functors-aeson directory
    distributive filepath hspec http-types kan-extensions lens mtl
    resource-pool scientific scotty semigroups split sqlite-simple
    template-haskell text time transformers utf8-string vector wai
    wai-cors wai-extra wai-middleware-static warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec mtl sqlite-simple ];
  license = stdenv.lib.licenses.bsd3;
}
