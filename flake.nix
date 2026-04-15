{
  description = "izumi-reflect build environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/25.11";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mudyla.url = "github:7mind/mudyla";
  inputs.mudyla.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , mudyla
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs.buildPackages; [
            ncurses

            coursier
            sbt
            bazel

            nodejs
            nodePackages.npm

            clang
            llvmPackages.libcxxClang

            python3

            gitMinimal
            gnupg

            mudyla.packages.${system}.default
          ];

          shellHook = ''
            export JDK11=${pkgs.jdk11_headless}
            export JDK17=${pkgs.jdk17_headless}
            export JDK21=${pkgs.jdk21_headless}
            export JDK_DEV=${pkgs.graalvmPackages.graalvm-ce}

            export JAVA_HOME=''${JDK_DEV}
            export PATH="''${JDK_DEV}/bin:$PATH"

            # Create .env directory with JDK symlink (ignore errors if already exists)
            mkdir -p ./.env 2>/dev/null || true
            rm -f ./.env/jdk 2>/dev/null || true
            ln -sf ''${JDK_DEV} ./.env/jdk 2>/dev/null || true
          '';
        };
      }
    );
}
