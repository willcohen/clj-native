{
  description = "clj-native: shared native interop infrastructure (dispatch, resource tracking, WASM lifecycle, build helpers, codegen) for Clojure libraries binding C and Java-via-web-image libraries.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Detect actual container arch when running inside a nixos container
        # (host may be aarch64-darwin, container may be x86_64-linux).
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

        # Build tools run on the build machine, so their libc does not
        # matter. They come from pkgs, which the binary cache has: from
        # pkgsMusl, curl alone pulled a musl Python and meson into each Linux
        # shell, built from source.
        baseBuildInputs = with pkgs; [
          autoconf
          automake
          cmake
          curl
          gawk
          libtool
          pkg-config
        ];

        # zig builds the Linux and Windows libs from any host
        # (build/zig-toolchain!). readelf, for build/check-linux-lib!, and
        # llvm-readobj, for build/check-windows-lib!, come alone: the rest of
        # binutils would shadow the ld and ar of macOS.
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

        # Build the standard set of devShells for a Clojure library that
        # binds a C dependency: { default }, which builds the Linux and
        # Windows libs with zig.
        #
        # Parameters:
        #   jdk                JDK used for JAVA_HOME and to build the clojure CLI.
        #                      Pass graalvmPackages.graalvm-ce for native-image workflows.
        #   extraBuildInputs   Extra build tools (drawn from pkgs).
        #   extraDevInputs     Extra dev tools (drawn from pkgs). They come first
        #                      in PATH, so a caller's version wins.
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
        # nodejs_26, not the default nodejs (v24 LTS): v24 has a libuv kqueue
        # regression that aborts the host when a multi-worker pool that did
        # network I/O is torn down; v22 and v26 are clean.
        devShells = mkCrossShells { extraDevInputs = [ pkgs.nodejs_26 ]; };

        lib = {
          inherit mkCrossShells actualSystem;
        };
      }
    );
}
