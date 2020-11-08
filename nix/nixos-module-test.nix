{ pkgs ? import ./pkgs.nix }:
let
  habitscipline-production = import (./nixos-module.nix) { envname = "production"; };
  port = 8001;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "habitscipline-module-test";
    machine = {
      imports = [
        habitscipline-production
      ];
      services.habitscipline.production = {
        enable = true;
        api-server = {
          enable = true;
          inherit port;
        };
      };
    };
    testScript = ''
      machine.wait_for_unit("multi-user.target")

      machine.wait_for_open_port(${builtins.toString port})
      machine.succeed("curl localhost:${builtins.toString port}")
    '';
  }
)
