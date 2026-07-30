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

        # Use musl for Linux C builds so produced artifacts are distro-portable.
        buildPkgs = if pkgs.stdenv.hostPlatform.isLinux then pkgs.pkgsMusl else pkgs;

        # Shared C build-tool stack. cmake stays on glibc to avoid musl test failures.
        baseBuildInputs = with buildPkgs; [
          autoconf
          automake
          curl
          gawk
          gnu-config
          libtool
          pkg-config
        ] ++ [ pkgs.cmake ];

        # Cross-compile target package sets (mingw + musl are Linux-hosted).
        crossPkgs =
          if pkgs.stdenv.hostPlatform.isLinux then {
            linuxAmd64   = pkgs.pkgsCross.musl64;
            linuxAarch64 = pkgs.pkgsCross.aarch64-multiplatform-musl;
            windowsAmd64 = pkgs.pkgsCross.mingwW64;
            windowsArm64 = pkgs.pkgsCross.aarch64-w64-mingw32;
          } else {};

        # Build the standard set of devShells for a Clojure library that
        # binds a C dependency. Returns { default } on all systems, and on
        # Linux hosts, where the cross toolchains are available, additionally
        # { linuxAmd64Cross, linuxAarch64Cross, windowsAmd64Cross,
        # windowsArm64Cross }. The Containerfile's NIX_SHELL_* defaults name
        # those four.
        #
        # Parameters:
        #   jdk                JDK used for JAVA_HOME and to build the clojure CLI.
        #                      Pass graalvmPackages.graalvm-ce for native-image workflows.
        #   extraBuildInputs   Extra C-library deps (drawn from buildPkgs; musl on Linux).
        #   extraDevInputs     Extra dev tools (drawn from pkgs). Placed before the
        #                      base dev inputs so duplicates resolve in favor of the
        #                      caller's choice in PATH.
        #   extraShellHook     Appended to every shell's shellHook.
        mkCrossShells = {
          jdk ? pkgs.jdk25,
          extraBuildInputs ? [],
          extraDevInputs ? [],
          extraShellHook ? "",
        }: let
          clojurePkg = pkgs.clojure.override { inherit jdk; };

          baseDevInputs = [
            pkgs.babashka
            clojurePkg
            pkgs.clj-kondo
            pkgs.clojure-lsp
            jdk
            pkgs.maven
            pkgs.ripgrep
          ];

          commonHook = ''
            export JAVA_HOME=${jdk}
            export PATH="${jdk}/bin:$PATH"
          '' + extraShellHook;

          libBuildInputs = baseBuildInputs ++ extraBuildInputs;
          # extraDevInputs first so caller packages win PATH resolution.
          libDevInputs = extraDevInputs ++ baseDevInputs;
        in {
          default = pkgs.mkShell {
            buildInputs = libBuildInputs ++ libDevInputs;
            shellHook = commonHook;
          };

        } // (pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          # x86_64-linux-musl cross shell. Works whether the host is x86_64
          # or aarch64 Linux, because pkgsCross.musl64 always provides an
          # x86_64-unknown-linux-musl-gcc wrapper. Replaces the old .#build
          # shell, which assumed an x86_64 host.
          linuxAmd64Cross = pkgs.mkShell {
            buildInputs = libBuildInputs ++ libDevInputs ++ [
              crossPkgs.linuxAmd64.buildPackages.gcc
            ];
            shellHook = commonHook + ''
              export CC=${crossPkgs.linuxAmd64.buildPackages.gcc}/bin/x86_64-unknown-linux-musl-gcc
              export CXX=${crossPkgs.linuxAmd64.buildPackages.gcc}/bin/x86_64-unknown-linux-musl-g++
              export AR=${crossPkgs.linuxAmd64.buildPackages.gcc}/bin/x86_64-unknown-linux-musl-ar
              export RANLIB=${crossPkgs.linuxAmd64.buildPackages.gcc}/bin/x86_64-unknown-linux-musl-ranlib
              export CFLAGS="-fPIC -D_GNU_SOURCE $CFLAGS"
              export CXXFLAGS="-fPIC -D_GNU_SOURCE $CXXFLAGS"
              export LDFLAGS="-static-libgcc -static-libstdc++ $LDFLAGS"
              export CMAKE_SHARED_LINKER_FLAGS="-static-libgcc -static-libstdc++ -Wl,-Bstatic -lc -lm -lpthread -ldl -Wl,-Bdynamic"
            '';
          };

          linuxAarch64Cross = pkgs.mkShell {
            buildInputs = libBuildInputs ++ libDevInputs ++ [
              crossPkgs.linuxAarch64.buildPackages.gcc
            ];
            shellHook = commonHook + ''
              export CC=${crossPkgs.linuxAarch64.buildPackages.gcc}/bin/aarch64-unknown-linux-musl-gcc
              export CXX=${crossPkgs.linuxAarch64.buildPackages.gcc}/bin/aarch64-unknown-linux-musl-g++
              export AR=${crossPkgs.linuxAarch64.buildPackages.gcc}/bin/aarch64-unknown-linux-musl-ar
              export RANLIB=${crossPkgs.linuxAarch64.buildPackages.gcc}/bin/aarch64-unknown-linux-musl-ranlib
              export CFLAGS="-fPIC -D_GNU_SOURCE $CFLAGS"
              export CXXFLAGS="-fPIC -D_GNU_SOURCE $CXXFLAGS"
              export LDFLAGS="-static-libgcc -static-libstdc++ $LDFLAGS"
              export CMAKE_SHARED_LINKER_FLAGS="-static-libgcc -static-libstdc++ -Wl,-Bstatic -lc -lm -lpthread -ldl -Wl,-Bdynamic"
            '';
          };

          windowsAmd64Cross = pkgs.mkShell {
            buildInputs = libBuildInputs ++ libDevInputs ++ [
              crossPkgs.windowsAmd64.buildPackages.gcc
              crossPkgs.windowsAmd64.windows.pthreads
            ];
            shellHook = commonHook + ''
              export CC=${crossPkgs.windowsAmd64.buildPackages.gcc}/bin/x86_64-w64-mingw32-gcc
              export CXX=${crossPkgs.windowsAmd64.buildPackages.gcc}/bin/x86_64-w64-mingw32-g++
              export AR=${crossPkgs.windowsAmd64.buildPackages.gcc}/bin/x86_64-w64-mingw32-ar
              export RANLIB=${crossPkgs.windowsAmd64.buildPackages.gcc}/bin/x86_64-w64-mingw32-ranlib
              export CFLAGS="-static-libgcc -static-libstdc++ $CFLAGS"
              export CXXFLAGS="-static-libgcc -static-libstdc++ $CXXFLAGS"
              export LDFLAGS="-static-libgcc -static-libstdc++ -Wl,--as-needed $LDFLAGS"
            '';
          };

          windowsArm64Cross = pkgs.mkShell {
            buildInputs = libBuildInputs ++ libDevInputs ++ [
              crossPkgs.windowsArm64.buildPackages.gcc
              crossPkgs.windowsArm64.windows.pthreads
            ];
            shellHook = commonHook + ''
              export CC=${crossPkgs.windowsArm64.buildPackages.gcc}/bin/aarch64-w64-mingw32-gcc
              export CXX=${crossPkgs.windowsArm64.buildPackages.gcc}/bin/aarch64-w64-mingw32-g++
              export AR=${crossPkgs.windowsArm64.buildPackages.gcc}/bin/aarch64-w64-mingw32-ar
              export RANLIB=${crossPkgs.windowsArm64.buildPackages.gcc}/bin/aarch64-w64-mingw32-ranlib
              export CFLAGS="-static-libgcc -static-libstdc++ $CFLAGS"
              export CXXFLAGS="-static-libgcc -static-libstdc++ $CXXFLAGS"
              export LDFLAGS="-static-libgcc -static-libstdc++ -Wl,--as-needed $LDFLAGS"
            '';
          };
        });

        # clj-native's own dev shell: the Clojure toolchain plus Node for the
        # squint/cljs test lane. nodejs_26 (not the default nodejs, currently
        # v24 LTS): v24 has a libuv kqueue regression that aborts the host when
        # a multi-worker pool that did network I/O is torn down; v22 and v26 are
        # clean.
        devInputs = with pkgs; [
          babashka
          clojure
          clj-kondo
          clojure-lsp
          jdk25
          maven
          nodejs_26
          ripgrep
        ];
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = devInputs;
          shellHook = ''
            export JAVA_HOME=${pkgs.jdk25}
            export PATH="${pkgs.jdk25}/bin:$PATH"
          '';
        };

        lib = {
          inherit mkCrossShells baseBuildInputs crossPkgs buildPkgs actualSystem;
        };
      }
    );
}
