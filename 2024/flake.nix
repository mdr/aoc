{
  description = "Advent of Code 2024 (Lean 4)";

  # elan installs the exact Lean release named in lean-toolchain (v4.13.0).
  # nixpkgs only had lean4 4.13.0 at an uncached commit, so using it would
  # mean building Lean from source.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          # go-task and python3 are for Taskfile.yml and the add_day_to_*.py scripts.
          packages = with pkgs; [ elan go-task python3 ];
        };
      });
    };
}
