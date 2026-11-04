{
  description = "Advent of Code 2015 and 2021 (Scala 3)";

  # nixpkgs 21.11: JDK 11 (which Scala 3.1 and sbt 1.5 support) and an sbt launcher that runs on it.
  # sbt itself comes from project/build.properties.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [ pkgs.jdk11 (pkgs.sbt.override { jre = pkgs.jdk11; }) ];
        };
      });
    };
}
