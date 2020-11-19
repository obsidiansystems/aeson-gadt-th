{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck;
  dep-sum-overrides = self: super: {
    dependent-sum-template = self.callHackageDirect {
      pkg = "dependent-sum-template";
      ver = "0.1.0.3";
      sha256 = "0m5nblmwbx2810hhnlcz1c8vwis47kd3xir1ylfk0dgxa0n1ag3f";
    } {};
    dependent-sum = self.callHackageDirect {
      pkg = "dependent-sum";
      ver = "0.7.1.0";
      sha256 = "0jjdjhkhny8hiz9q17bqdgncca8gb0nqdnqz3xpwa3g2g0qisrp0";
    } {};
    some = doJailbreak super.some;
    dependent-map = self.callHackageDirect {
      pkg = "dependent-map";
      ver = "0.4.0.0";
      sha256 = "1jycg6hz350mjbiqnqii90k3fbz95rbwd3kw09n4x9r053bbz3jn";
    } {};
  };
  ghc810-overrides = self: super: dep-sum-overrides self super // {
    th-expand-syns = self.callHackageDirect {
      pkg = "th-expand-syns";
      ver = "0.4.6.0";
      sha256 = "1l2g98jfg86blp8mkkvzh90h557l5qklw1nn045zqb5am8977dgq";
    } {};
    ChasingBottoms = self.callHackageDirect {
      pkg = "ChasingBottoms";
      ver = "1.3.1.8";
      sha256 = "0klxmb6pgl2xv5206gn2m3n1di2aidkfyi5rlqcfdx5qvpbnhl19";
    } {};
    haskell-src-meta = self.callHackageDirect {
      pkg = "haskell-src-meta";
      ver = "0.8.5";
      sha256 = "1dhncvsyv2kc8x18gvr7if4pr7vvypl0lr450jaaj3xj7rly3lwv";
    } {};
    haskell-src-exts = self.callHackageDirect {
      pkg = "haskell-src-exts";
      ver = "1.22.0";
      sha256 = "1w1fzpid798b5h090pwpz7n4yyxw4hq3l4r493ygyr879dvjlr8d";
    } {};
    constraints = self.callHackageDirect {
      pkg = "constraints";
      ver = "0.11";
      sha256 = "0xi2p57hsdy31f8a4isxxp1zgv8m7a26c586jlz8p2rmk0ypw3pj";
    } {};
    constraints-extras = doJailbreak super.constraints-extras;
    some = doJailbreak super.some;
  };
  ghcs = rec {
    ghc802 = nixos1809.haskell.packages.ghc802;
    ghc844 = nixos1809.haskell.packages.ghc844;
    ghc865 = nixos2003.haskell.packages.ghc865;
    ghc884 = nixos2003.haskell.packages.ghc884.override {
      overrides = dep-sum-overrides;
    };
    ghc8101 = nixos2003.haskell.packages.ghc8101.override {
      overrides = ghc810-overrides;
    };
    ghc8102 = unstable.haskell.packages.ghc8102;
    ghc8101_aeson15 = nixos2003.haskell.packages.ghc8101.override {
      overrides = self: super: ghc810-overrides self super //
      { assoc = doJailbreak super.assoc;
        strict = self.callHackageDirect {
          pkg = "strict";
          ver = "0.4";
          sha256 = "0sl9mfpnyras2jlpjfnji4406fzp0yg2kxfcr22s3zwpir622a97";
        } {};
        these = self.callHackageDirect {
          pkg = "these";
          ver = "1.1.1.1";
          sha256 = "1i1nfh41vflvqxi8w8n2s35ymx2z9119dg5zmd2r23ya7vwvaka1";
        } {};
        aeson = doJailbreak (self.callHackageDirect {
          pkg = "aeson";
          ver = "1.5.2.0";
          sha256 = "0rz7j7bcj5li2c5dmiv3pnmbs581vzkl9rbx9wq2v06f4knaklkf";
        } {});
        hashable-time = doJailbreak super.hashable-time;
        Diff = self.callHackageDirect {
          pkg = "Diff";
          ver = "0.4.0";
          sha256 = "1phz4cz7i53jx3d1bj0xnx8vpkk482g4ph044zv5c6ssirnzq3ng";
        } {};
        doctest = dontCheck (self.callHackageDirect {
          pkg = "doctest";
          ver = "0.16.3";
          sha256 = "0rm91akq3d4b8xa127dklgd1vg2x9xv962pg98i7xhgnllp6i5r3";
        } {});
        quickcheck-instances = self.callHackageDirect {
          pkg = "quickcheck-instances";
          ver = "0.3.23";
          sha256 = "1baqh2harkcx7igqmk6p040vmchy30wnh1crwwvzcxqv22iwyfrw";
        } {};
      };
    };
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "aeson-gadt-th" ./. {}) ghcs
