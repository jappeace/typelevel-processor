{ sources ? import ../npins }:
{
  # The library holds every component proof and the test suite runs programs on
  # the processor, so a successful build is the whole check.
  native = import ../default.nix { hpkgs = import ./hpkgs.nix { pkgs = import ./pkgs.nix { inherit sources; }; }; };
}
