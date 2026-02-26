{
  description = "lila development environment for Nix & flakes";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; # unstable Nixpkgs

  outputs =
    { self, ... }@inputs:

    let
      javaVersion = 21;
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forEachSupportedSystem =
        f:
        inputs.nixpkgs.lib.genAttrs supportedSystems (
          system:
          f {
            pkgs = import inputs.nixpkgs {
              inherit system;
              overlays = [ inputs.self.overlays.default ];
            };
          }
        );
    in
    {
      overlays.default =
        final: prev:
        let
          jdk = prev."jdk${toString javaVersion}";
        in
        rec {
          java = jdk;
          sbt = prev.sbt.override { jre = jdk; };
          scala = prev.scala_3.override { jre = jdk; };
        };

      devShells = forEachSupportedSystem (
        { pkgs }:
        {
          default = pkgs.mkShellNoCC {
            packages = with pkgs; [
              java
              scala
              sbt
              coursier
            ];
          };
        }
      );
    };
}
