let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay =
    import (
      pkgs.fetchFromGitHub (import ./validity-version.nix) + "/nix/overlay.nix"
    );
  typed-uuid-overlay =
    import (
      pkgs.fetchFromGitHub (import ./typed-uuid-version.nix) + "/nix/overlay.nix"
    );
  cursor-overlay =
    import (
      pkgs.fetchFromGitHub (import ./cursor-version.nix) + "/nix/overlay.nix"
    );
  cursor-brick-overlay =
    import (
      pkgs.fetchFromGitHub (import ./cursor-brick-version.nix) + "/nix/overlay.nix"
    );
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  appendful-overlay =
    import (
      pkgs.fetchFromGitHub (import ./appendful-version.nix) + "/nix/overlay.nix"
    );
  mergeful-overlay =
    import (
      pkgs.fetchFromGitHub (import ./mergeful-version.nix) + "/nix/overlay.nix"
    );
  hastoryPkgs =
    pkgsv {
      overlays =
        [
          validity-overlay
          typed-uuid-overlay
          cursor-overlay
          cursor-brick-overlay
          yamlparse-applicative-overlay
          appendful-overlay
          mergeful-overlay
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
hastoryPkgs
