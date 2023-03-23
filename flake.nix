{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let supportedSystems = [ flake-utils.lib.system.aarch64-darwin ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        # https://serokell.io/blog/practical-nix-flakes
        haskellPackages = pkgs.haskellPackages;
        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
        packageName = "housekeeper";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName ./housekeeper rec { };

        defaultPackage = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {

          buildInputs = with pkgs; [
            cabal-install
            ghc
            haskell-language-server
            postgresql
            ormolu
            haskellPackages.fourmolu
            zlib
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
