{
  description = "Advent of Code development environment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            # 2015 (C), 2016 (C++)
            gcc

            # 2017 (Ballerina)
            ballerina

            # 2018 (Common Lisp)
            clisp

            # 2019 (Ruby)
            ruby

            # 2020 (R)
            R

            # 2021 (Python)
            python313

            # 2022 (JavaScript)
            nodejs_22

            # 2023 (Rust)
            rustc
            cargo

            # 2024 (Lua)
            lua

            # 2025 (Haskell)
            ghc
          ];
        };
      }
    );
}
