{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, easy-purescript-nix }:
    let supportedSystems = [ flake-utils.lib.system.aarch64-darwin ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        easy-ps = import easy-purescript-nix { inherit pkgs; };
        # https://serokell.io/blog/practical-nix-flakes
        haskellPackages = pkgs.haskellPackages;
        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
        packageName = "housekeeper";
      in {
        packages = {
          ${packageName} =
            haskellPackages.callCabal2nix packageName ./housekeeper rec { };
          "${packageName}Test" =
            haskellPackages.callCabal2nix packageName ./housekeeper rec { };

        };

        defaultPackage = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {

          buildInputs = (with pkgs; [
            cabal-install
            ghc
            haskell-language-server
            postgresql
            ormolu
            haskellPackages.fourmolu
            zlib

            nodejs
          ]) ++ (with easy-ps; [
            purs
            purs-tidy
            psa
            spago
            purescript-language-server
          ]) ++ (pkgs.lib.optionals (system == "aarch64-darwin")
            (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa CoreServices ]));

          inputsFrom = builtins.attrValues self.packages.${system};

          HSPEC_COLOR = "yes";
          HSPEC_FORMAT = "progress";
        };
      });
}
