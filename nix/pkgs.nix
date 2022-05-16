{ sources ? import ./sources.nix
}:
import sources.nixpkgs {
  overlays =
    [
      (import (sources.appendful + "/nix/overlay.nix"))
      (import (sources.appendful + "/nix/overlay.nix"))
      (import (sources.autodocodec + "/nix/overlay.nix"))
      (import (sources.cursor + "/nix/overlay.nix"))
      (import (sources.cursor-brick + "/nix/overlay.nix"))
      (import (sources.mergeful + "/nix/overlay.nix"))
      (import (sources.safe-coloured-text + "/nix/overlay.nix"))
      (import (sources.sydtest + "/nix/overlay.nix"))
      (import (sources.typed-uuid + "/nix/overlay.nix"))
      (import (sources.validity + "/nix/overlay.nix"))
      (import ./overlay.nix)
      (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
    ];
  config.allowUnfree = true;
}
