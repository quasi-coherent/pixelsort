{ compiler ? "ghc865"
, nixpkgs ? import (import ./nix/sources.nix).nixpkgs {}
}:

let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  hsPkgs = nixpkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "pixelsort" =
        self.callCabal2nix
          "pixelsort"
          (gitignore ./.)
          {};
    };
  };

  shell = hsPkgs.shellFor {
    packages = ps: [
      ps."pixelsort"
    ];

    buildInputs = with nixpkgs.haskellPackages; [
      ghcid
      hlint
      hpack
      hsPkgs.cabal-install
      stack
      stylish-haskell
    ];

    withHoogle = true;
  };

  exe = nixpkgs.haskell.lib.justStaticExecutables (hsPkgs."pixelsort");

in { inherit shell; inherit exe; }
