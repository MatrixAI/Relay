{
  pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/6d8fea6668b98dbed2e0c0bb9c5d7edebc417341.tar.gz) {}
}:
  with pkgs;
  haskell.lib.buildStackProject {
    name = "automaton_networking_and_composition_experiment";
    src = null;
    buildInputs = [ python3 ];
    shellHook = ''
      echo 'Entering experiment environment'
      set -v

      alias stack='\stack --nix'

      set +v
    '';
  }
