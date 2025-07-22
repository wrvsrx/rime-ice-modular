{
  description = "Modular rime-ice";

  inputs = {
    nixpkgs.url = "github:wrvsrx/nixpkgs/patched-nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { inputs, ... }:
      {
        systems = [ "x86_64-linux" ];
        perSystem =
          { pkgs, ... }:
          rec {
            packages =
              let
                default = pkgs.callPackage ./default.nix { };
                components' = import ./components.nix {
                  rime-ice-modular = default;
                  stdenv = pkgs.stdenv;
                };
              in
              {
                inherit default;
                inherit (components')
                  rime-ice-pinyin
                  ;
              };
            devShells.default = pkgs.mkShell { inputsFrom = [ packages.default ]; };
            formatter = pkgs.nixfmt-rfc-style;
          };
      }
    );
}
