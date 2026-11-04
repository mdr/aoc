{
  description = "Advent of Code 2022 (Idris 2)";

  # The nixpkgs revision the original shell.nix pinned (Idris 2 0.6.0).
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/ce5fe99df1f15a09a91a86be9738d68fadfbad82";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [ idris2 chez-racket rlwrap ];
        };
      });
    };
}
