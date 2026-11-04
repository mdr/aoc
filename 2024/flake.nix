{
  description = "Advent of Code 2024 (Lean 4)";

  # Lean comes from nixpkgs. Keep lean-toolchain (used by elan, e.g. in editors)
  # at the same version when updating flake.lock.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            pkgs.lean4
            # For Taskfile.yml and the add_day_to_*.py scripts.
            pkgs.go-task
            (pkgs.python3.withPackages (ps: [ ps.toml ps.pyyaml ]))
          ];
        };
      });
    };
}
