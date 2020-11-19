{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003;
  inherit (nixos2003) lib;
  ghcs = {
    ghc802 = nixos1809.haskell.packages.ghc802;
    ghc844 = nixos1809.haskell.packages.ghc844;
    ghc865 = nixos2003.haskell.packages.ghc865;
    ghc884 = nixos2003.haskell.packages.ghc884.override {
      overrides = self: super: {
        template-haskell = super.template-haskell_2_15_0_0;
      };
    };
    ghc8101 = nixos2003.haskell.packages.ghc884.override {
      overrides = self: super: {
        template-haskell = super.template-haskell_2_15_0_0;
      };
    };
  };
in 
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "aeson-gadt-th" ./. {}) ghcs
