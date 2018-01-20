{ pkgs ? import <nixpkgs> {} }: pkgs.haskell.packages.ghc802.developPackage {
  root = ./.; 
  overrides = self: super: {
    cgi = pkgs.haskell.lib.doJailbreak super.cgi;
  };
  #source-overrides = { 
  #  cgi = pkgs.fetchFromGitHub {
  #    owner = "cheecheeo";
  #    repo = "haskell-cgi";
  #    rev = "e1387d6c49c71d483946257b506e27c5dfab8189";
  #    sha256 = "17ky4qdc8sqsnr1yq4gcbjn3wai6wbv2kbisr7j8v4bg68r5rp2a";
  #  };
  #};
}
