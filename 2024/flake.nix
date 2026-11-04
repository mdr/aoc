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
        default = pkgs.mkShell ({
          packages = [
            pkgs.lean4
            # For Taskfile.yml and the add_day_to_*.py scripts.
            pkgs.go-task
            (pkgs.python3.withPackages (ps: [ ps.toml ps.pyyaml ]))
          ];
        } // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          # nixpkgs' Lean doesn't bundle clang and lld like official releases,
          # so Lake falls back to $CC (gcc and GNU ld), which leaves symbols
          # from UnicodeBasic's C library out of its precompiled modules.
          LEAN_CC = toString (pkgs.writeShellScript "lean-cc" ''
            PATH=${pkgs.lld}/bin:$PATH exec ${pkgs.stdenv.cc}/bin/cc -fuse-ld=lld "$@"
          '');
        });
      });
    };
}
