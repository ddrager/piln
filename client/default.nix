{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "01d1f776818176a9cac7d7c9d3a319694fea4f1a";
    sha256 = "0mdnld55395xk2vwh500mgq7r7pklsi4fx6qakkmdxwzyi4sg206";
  }){}
}:
let
  patchedghcjs = pkgs.lib.overrideDerivation pkgs.haskell.compiler.ghcjs (drv: {
    patches = [ ./680.patch ];
  });

  ghcjspkgs = pkgs.haskell.packages.ghcjs.override {
    ghc = patchedghcjs;
    overrides = self: super: {
      http-types = pkgs.haskell.lib.dontCheck super.http-types;
      http-media = pkgs.haskell.lib.dontCheck super.http-media;
      servant = pkgs.haskell.lib.dontCheck super.servant;
    };
  };

  result = ghcjspkgs.callCabal2nix "miso" (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "bb2be3264ff3c6aa3b18e471d7cf04296024059b";
    sha256 = "07k1rlvl9g027fp2khl9kiwla4rcn9sv8v2dzm0rzf149aal93vn";
  }) {};
in ghcjspkgs.callPackage ./app.nix {
  miso = result;
}
