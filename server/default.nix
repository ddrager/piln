let
  pkgs = import <nixpkgs> { };
  hp = pkgs.haskellPackages.extend (self: super: {
    rebase = super.rebase.override {
      rerebase = self.rerebase_1_3;
    };
    text-builder = pkgs.haskell.lib.dontCheck super.text-builder;
  });
in
  hp.callPackage ./app.nix { }
