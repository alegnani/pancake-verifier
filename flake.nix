{
  description = "Rust overlay";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        rust = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "cargo" "rustc"];
        }; 
      in
      with pkgs;
      {
        devShells.default = mkShell rec {
          buildInputs = [
            rust   
            icu
            openssl
            hol
            gnumake
            polyml
            jdk11
            z3
          ];

          # change this to your local installation of Viper and cake
          VIPER_INSTALL = "/home/legna/.config/Code/User/globalStorage/viper-admin.viper/Stable/ViperTools";
          # first looks for the cake binary at CAKE_ML, then falls back to executables on PATH
          # CAKE_ML="";

          VIPER_HOME = "${VIPER_INSTALL}/backends";
          JAVA_HOME = "${jdk11}";
          Z3_EXE = "${VIPER_INSTALL}/z3/bin/z3";

          LD_LIBRARY_PATH = "${jdk11}/lib/openjdk/lib/server:${lib.makeLibraryPath buildInputs}";
          nativeBuildInputs = [ rustPlatform.bindgenHook ];
          RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/library";

          NIX_LD_LIBRARY_PATH = lib.makeLibraryPath [
            stdenv.cc.cc
            stdenv.cc.cc.lib
          ];
        };
      }
    );
}
