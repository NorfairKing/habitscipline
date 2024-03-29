{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "habitscipline-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    haskellPackages.autoexporter
    killall
    unzip
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
