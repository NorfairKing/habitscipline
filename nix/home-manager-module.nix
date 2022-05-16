{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.habitscipline;


in
{
  options =
    {
      programs.habitscipline =
        {
          enable = mkEnableOption "Habitscipline cli and syncing";
          extraConfig =
            mkOption {
              type = types.str;
              description = "Extra contents for the config file";
              default = "";
            };
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Habitscipline syncing";
                        server-url =
                          mkOption {
                            type = types.str;
                            example = "api.habitscipline.cs-syd.eu";
                            description = "The url of the sync server";
                          };
                        username =
                          mkOption {
                            type = types.str;
                            example = "syd";
                            description =
                              "The username to use when logging into the sync server";
                          };
                        password =
                          mkOption {
                            type = types.str;
                            example = "hunter12";
                            description =
                              "The password to use when logging into the sync server";
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      habitsciplinePkgs = (import ./pkgs.nix { }).habitsciplinePackages;
      configContents = cfg: ''
        

      '';
      syncConfigContents = syncCfg:
        optionalString (syncCfg.enable or false) ''

server-url: "${cfg.sync.server-url}"
username: "${cfg.sync.username}"
password: "${cfg.sync.password}"

      '';


      syncHabitsciplineName = "sync-habitscipline";
      syncHabitsciplineService =
        {
          Unit =
            {
              Description = "Sync habitscipline";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "sync-habitscipline-service-ExecStart"
                  ''
                    exec ${habitsciplinePkgs.habitscipline-cli}/bin/habitscipline-cli sync
                  ''}";
              Type = "oneshot";
            };
        };
      syncHabitsciplineTimer =
        {
          Unit =
            {
              Description = "Sync habitscipline every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "daily";
              Persistent = true;
              Unit = "${syncHabitsciplineName}.service";
            };
        };

      habitsciplineConfigContents =
        concatStringsSep "\n" [
          (configContents cfg)
          (syncConfigContents cfg.sync)
        ];

      services =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncHabitsciplineName}" = syncHabitsciplineService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncHabitsciplineName}" = syncHabitsciplineTimer;
          }
        );
      packages =
        [
          habitsciplinePkgs.habitscipline-cli
          habitsciplinePkgs.habitscipline-tui
        ];


    in
    mkIf cfg.enable {
      xdg = {
        configFile."habitscipline/config.yaml".text = habitsciplineConfigContents;
      };
      systemd.user =
        {
          startServices = true;
          services = services;
          timers = timers;
        };
      home.packages = packages;
    };
}
