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
        rust = pkgs.rust-bin.nightly.latest.default.override {
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
            scala_2_13
          ];

          LD_LIBRARY_PATH = "${lib.makeLibraryPath buildInputs}";
          # LIBCLANG_PATH = pkgs.lib.makeLibraryPath [ pkgs.llvmPackages_latest.libclang.lib ];
          nativeBuildInputs = [ rustPlatform.bindgenHook ];

          RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/library";
          shellHook = ''
            export PATH=$(realpath ../cake-x64-64):$PATH
            export PANCAKE_LSP=$(realpath ./target/debug/pancake-language-server)
          '';

          NIX_LD_LIBRARY_PATH = lib.makeLibraryPath [
            stdenv.cc.cc
            stdenv.cc.cc.lib
          ];
        };
      }
    );
}
