{ pkgs ? import ./pkgs.nix { }
,
}:
let
  # Decision: use lib.fileset.toSource for fine grained source filtering.
  # Alternatives considered: passing ../. directly (rebuilds on any nix/tooling
  # change), cleanSource (still includes too much). fileset.unions makes the
  # build inputs explicit so editing nix/, .github/, makefile, .hlint.yaml,
  # etc. does not invalidate the haskell build cache.
  src = pkgs.lib.fileset.toSource {
    root = ../.;
    fileset = pkgs.lib.fileset.unions [
      ../app
      ../src
      ../test
      ../typelevel-processor.cabal
      ../LICENSE
      ../Readme.md
      ../Changelog.md
    ];
  };
in
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    typelevel-processor = hnew.callCabal2nix "typelevel-processor" src { };
  };
}
