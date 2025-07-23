{
  description = "Modular rime-ice";

  inputs = {
    nixpkgs.url = "github:wrvsrx/nixpkgs/patched-nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nur-wrvsrx = {
      url = "github:wrvsrx/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { inputs, ... }:
      {
        systems = [ "x86_64-linux" ];
        perSystem =
          { pkgs, system, ... }:
          rec {
            _module.args.pkgs = import inputs.nixpkgs {
              inherit system;
              overlays = [ inputs.nur-wrvsrx.overlays.default ];
            };
            packages =
              let
                rime-ice-modular-src = pkgs.callPackage ./rime-ice-modular-src.nix {
                  source = {
                    pname = "rime-ice-modular";
                    src = ./.;
                    version = "2025.04.26-04";
                  };
                };
                components' = import ./components.nix {
                  inherit rime-ice-modular-src;
                  inherit (pkgs.rimePackages) callPackage;
                };
              in
              {
                default = rime-ice-modular-src;
                inherit rime-ice-modular-src;
              }
              // components';
            devShells.default = pkgs.mkShell { inputsFrom = [ packages.default ]; };
            formatter = pkgs.nixfmt-rfc-style;
          };
      }
    );
}
