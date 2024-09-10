{
  description = "Dev shell for rust nightly";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = [
            openssl
            pkg-config
            # 1.69
            #(rust-bin.nightly."2023-12-01".default.override {
            # 1.68.0
            #(rust-bin.nightly."2023-01-04".default.override {
            # 1.77.0
            #(rust-bin.nightly."2024-01-01".default.override {
            # 1.79.0
            (rust-bin.nightly."2024-04-01".default.override {
              extensions = [
                "rust-src"
                "rustc-dev"
                "rust-analyzer"
              ];
            })
          ];
        };
      }
    );
}
