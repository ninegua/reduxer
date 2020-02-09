{ pkgs ? import <nixpkgs> {} }: pkgs.haskell.packages.ghc882.developPackage {
  root = ./.; 
  overrides = self: super: {
    cgi = self.callPackage (
        { mkDerivation, base, bytestring, containers, doctest, exceptions
        , mtl, multipart, network, network-uri, parsec, QuickCheck, time, xhtml, stdenv
        }:
        mkDerivation {
          pname = "cgi";
          version = "3001.5.0.0";
          sha256 = "09wvp9vkqasns4flw9z46nhcy96r4qxjv6h47d5f90drz77pmm8a";
          libraryHaskellDepends = [
            base bytestring containers exceptions mtl multipart network network-uri
            parsec time xhtml
          ];
          testHaskellDepends = [ base doctest QuickCheck ];
          homepage = "https://github.com/cheecheeo/haskell-cgi";
          description = "A library for writing CGI programs";
          license = stdenv.lib.licenses.bsd3;
          configureFlags = "--flags -network-uri";
        }) {};

    multipart = self.callPackage ({ mkDerivation, base, bytestring, parsec, stringsearch, stdenv }:
     mkDerivation {
       pname = "multipart";
       version = "0.2.0";
       sha256 = "1rw668hxj04zpkfwhjqbd0ph0wy9k2khsrbsni9sxi2px49vnmym";
       libraryHaskellDepends = [ base bytestring parsec stringsearch ];
       description = "HTTP multipart split out of the cgi package";
       license = stdenv.lib.licenses.bsd3;
     }) {};
  };
  modifier = drv : pkgs.haskell.lib.appendConfigureFlag (pkgs.haskell.lib.justStaticExecutables drv) ''
    --ghc-option=-optl=-static
    --ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib
    --ghc-option=-optl=-L${pkgs.zlib.static}/lib
    --ghc-option=-optl=-L${pkgs.glibc.static}/lib
    --ghc-option=-optl=-L${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib
  '';
}
