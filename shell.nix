let
    sources = import ./nix/sources.nix;

    nixpkgs = import sources.nixpkgs {};

    stdenv = nixpkgs.stdenv;

in 
with nixpkgs.haskellPackages;
stdenv.mkDerivation {
  pname = "metro-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildInputs = [ nixpkgs.stack nixpkgs.cabal-install nixpkgs.zlib nixpkgs.bzip2 nixpkgs.lz4 nixpkgs.snappy nixpkgs.zstd nixpkgs.gmp nixpkgs.rocksdb ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/metro-backend#readme";
  license = "MIT";
}

