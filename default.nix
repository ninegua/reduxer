{ pkgs ? import <nixpkgs> {} }: pkgs.haskell.packages.ghc802.developPackage {
  root = ./.; 
  overrides = self: super: {
    cgi = pkgs.haskell.lib.doJailbreak super.cgi;
  };
  modifier = drv : pkgs.haskell.lib.appendConfigureFlag (pkgs.haskell.lib.justStaticExecutables drv) ''
    --ghc-option=-optl=-static
    --ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib
    --ghc-option=-optl=-L${pkgs.zlib.static}/lib
    --ghc-option=-optl=-L${pkgs.glibc.static}/lib
  '';
}
