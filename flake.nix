{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      packages.default = pkgs.polyml;

      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [ 
          hol
          gnumake
          polyml
        ];

        shellHook = ''
          export PATH=$(realpath ./cake-x64-64):$PATH
        '';
      };
    });
}

