{
  description = "clj-native: dispatch, WASM lifecycle, build helpers and codegen for Clojure libraries that bind a C library.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # In a nixos container, `system` can name the host (aarch64-darwin)
        # and not the container (x86_64-linux).
        actualSystem =
          if builtins.pathExists "/etc/os-release"
             && (builtins.match ".*ID=nixos.*" (builtins.readFile "/etc/os-release")) != null
          then
            let
              arch = builtins.readFile (builtins.toPath "/proc/sys/kernel/osrelease");
              isArm64 = builtins.match ".*aarch64.*" arch != null;
            in if isArm64 then "aarch64-linux" else "x86_64-linux"
          else system;

        pkgs = nixpkgs.legacyPackages.${actualSystem};

        baseBuildInputs = with pkgs; [
          autoconf
          automake
          cmake
          gawk
          libtool
          pkg-config
        ];

        # zig builds the Linux and Windows libs on any host. readelf and
        # llvm-readobj come alone, since the rest of binutils and LLVM would
        # shadow the macOS ld and ar.
        zigInputs = [
          pkgs.zig
          (pkgs.runCommand "readelf" { } ''
            mkdir -p $out/bin
            ln -s ${pkgs.binutils-unwrapped}/bin/readelf $out/bin/readelf
          '')
          (pkgs.runCommand "llvm-readobj" { } ''
            mkdir -p $out/bin
            ln -s ${pkgs.llvm}/bin/llvm-readobj $out/bin/llvm-readobj
          '')
        ];

        # The devShells of a Clojure library that binds a C library:
        # { default }, which builds the Linux and Windows libs with zig.
        #
        # Parameters:
        #   jdk               JAVA_HOME and the JDK of the clojure CLI. Pass
        #                     graalvmPackages.graalvm-ce for native-image.
        #   extraBuildInputs  More build tools.
        #   extraDevInputs    More dev tools. They come before the base dev
        #                     tools in PATH, so a caller's version of one wins.
        mkCrossShells = {
          jdk ? pkgs.jdk25,
          extraBuildInputs ? [],
          extraDevInputs ? [],
        }: {
          default = pkgs.mkShell {
            buildInputs = baseBuildInputs ++ zigInputs ++ extraBuildInputs
              ++ extraDevInputs
              ++ [
                pkgs.babashka
                (pkgs.clojure.override { inherit jdk; })
                pkgs.clj-kondo
                pkgs.clojure-lsp
                jdk
                pkgs.ripgrep
              ];
            shellHook = ''
              export JAVA_HOME=${jdk}
              export PATH="${jdk}/bin:$PATH"
            '';
          };
        };
      in {
        # Node 24 aborted on macOS at the shutdown of a multi-worker pool that
        # did network I/O (libuv kqueue). Not verified again.
        devShells = mkCrossShells { extraDevInputs = [ pkgs.nodejs_26 ]; };

        lib = {
          inherit mkCrossShells actualSystem;
        };
      }
    );
}
