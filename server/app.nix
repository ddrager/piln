{ mkDerivation, aeson, base, bytestring, contravariant, hasql
, http-types, lens, lens-aeson, scotty, stdenv, text, time, uuid
, wai-cors, wai-extra, wai-middleware-static, wreq
}:
mkDerivation {
  pname = "piln";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring contravariant hasql http-types lens
    lens-aeson scotty text time uuid wai-cors wai-extra
    wai-middleware-static wreq
  ];
  homepage = "https://github.com/githubuser/piln#readme";
  license = stdenv.lib.licenses.bsd3;
}
