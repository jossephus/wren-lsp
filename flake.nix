{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    zig.url = "github:mitchellh/zig-overlay";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    zig,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        overlays = [
          (final: prev: {
            zigpkgs = zig.packages.${system};
          })
        ];
        pkgs = import nixpkgs {
          inherit overlays system;
        };
        packages = with pkgs; [
          glfw
          libGL
          libxkbcommon
          pkg-config
          xorg.libxcb
          xorg.libXft
          xorg.libX11
          xorg.libX11.dev
          xorg.libXrandr
          xorg.libXinerama
          xorg.libXcursor
          xorg.libXi
          glfw-wayland
          zigpkgs."0.14.0"
        ];
      in {
        devShell = pkgs.mkShell {
          buildInputs = packages;
          nativeBuildInputs = with pkgs; [cmake pkg-config ncurses fontconfig freetype];
          shellHook = ''
            export SHELL=/usr/bin/bash
          '';
        };
      }
    );
}

