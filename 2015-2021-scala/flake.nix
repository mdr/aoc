{
  description = "Advent of Code 2015 and 2021 (Scala 3)";

  # sbt itself comes from project/build.properties; nixpkgs provides the launcher.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs:
        let jdk = pkgs.jdk25; in
        {
          default = pkgs.mkShellNoCC {
            packages = [ jdk (pkgs.sbt.override { jre = jdk; }) ];
          };
        });
    };
}
