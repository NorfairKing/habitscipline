{ pkgs ? import ./pkgs.nix }:
let
  sources = import ./sources.nix;
  habitscipline-production = import (./nixos-module.nix) { envname = "production"; };
  home-manager = import (sources.home-manager + "/nixos/default.nix");
  port = 8001;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "habitscipline-module-test";
    machine = {
      imports = [
        habitscipline-production
        home-manager
      ];
      services.habitscipline.production = {
        enable = true;
        api-server = {
          enable = true;
          inherit port;
        };
      };
      users.users.testuser.isNormalUser = true;
      home-manager.users.testuser = { pkgs, ... }: {
        imports = [
          ./home-manager-module.nix
        ];
        xdg.enable = true;
        programs.habitscipline = {
          enable = true;
          sync = {
            enable = true;
            server-url = "localhost:${builtins.toString port}";
            username = "testuser";
            password = "testpassword";
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      machine.wait_for_unit("multi-user.target")

      machine.wait_for_open_port(${builtins.toString port})
      machine.succeed("curl localhost:${builtins.toString port}")

      machine.wait_for_unit("home-manager-testuser.service")


      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      machine.succeed(su("testuser", "cat ~/.config/habitscipline/config.yaml"))

      machine.succeed(su("testuser", "habitscipline-cli register"))
      machine.succeed(su("testuser", "habitscipline-cli login"))
      machine.succeed(su("testuser", "habitscipline-cli sync"))
    '';
  }
)
