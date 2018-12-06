let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "c3e57f3095d3c5e4631a5978702226ee2b6bd8cd";
    sha256 = "0vb7ikjscrp2rw0dfw6pilxqpjm50l5qg2x2mn1vfh93dkl2aan7";
  };
  pkgs = import nixpkgs {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      pandoc = self.callHackage "pandoc" "2.2" {};
      pandoc-types = self.callHackage "pandoc-types" "1.17.4.2" {};
    };
  };
in pkgs.stdenv.mkDerivation {
  name = "csce_dfa_project_test";
  buildInputs = [ pkgs.haskell.compiler.ghc7103 ];
}
