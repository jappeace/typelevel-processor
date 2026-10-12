{
  hpkgs ? import ./nix/hpkgs.nix {},
  pkgs ? import ./nix/pkgs.nix {},
}:
hpkgs.shellFor {
  packages = ps: [ ps.typelevel-processor ];
  withHoogle = false;

  buildInputs = [
    pkgs.ghcid
    pkgs.cabal-install
  ];
}
