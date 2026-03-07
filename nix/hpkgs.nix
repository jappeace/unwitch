{ pkgs ? import ./pkgs.nix { }
,
}:
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    unwitch = hnew.callCabal2nix "unwitch" ../. { };
  };
}
