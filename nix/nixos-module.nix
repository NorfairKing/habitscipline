{ envname }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.habitscipline."${envname}";
  concatAttrs = attrList: fold (x: y: x // y) { } attrList;
in
{
  options.services.habitscipline."${envname}" =
    {
      enable = mkEnableOption "Habitscipline Service";
      api-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Habitscipline API Server";
                  log-level =
                    mkOption {
                      type = types.str;
                      example = "Debug";
                      default = "Warn";
                      description = "The log level to use";
                    };
                  hosts =
                    mkOption {
                      type = types.listOf (types.str);
                      default = [ ];
                      example = [ "api.habitscipline.cs-syd.eu" ];
                      description = "The host to serve api requests on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      default = 8000;
                      example = 8000;
                      description = "The port to serve api requests on";
                    };
                  local-backup =
                    mkOption {
                      type = types.nullOr (
                        types.submodule {
                          options = {
                            enable = mkEnableOption "Habitscipline API Server Local Backup Service";
                            backup-dir = mkOption {
                              type = types.str;
                              example = "backup/api-server";
                              default = "backup/api-server";
                              description = "The directory to store backups in, relative to the /www/habitscipline/${envname} directory or absolute";
                            };
                          };
                        }
                      );
                      default = null;
                    };
                };
            };
          default = null;
        };
    };
  config =
    let
      habitsciplinePkgs = (import ./pkgs.nix { }).habitsciplineReleasePackages;
      working-dir = "/www/habitscipline/${envname}/";
      # The docs server
      api-server-working-dir = working-dir + "api-server/";
      api-server-database-file = api-server-working-dir + "habitscipline-server-database.sqlite3";
      # The api server
      api-server-service =
        with cfg.api-server;
        optionalAttrs enable {
          "habitscipline-api-server-${envname}" = {
            description = "Habitscipline API Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "HABITSCIPLINE_API_SERVER_LOG_LEVEL" =
                  "${builtins.toString log-level}";
                "HABITSCIPLINE_API_SERVER_PORT" =
                  "${builtins.toString port}";
                "HABITSCIPLINE_API_SERVER_DATABASE" = api-server-database-file;
              };
            script =
              ''
                mkdir -p "${api-server-working-dir}"
                cd ${api-server-working-dir};
                ${habitsciplinePkgs.habitscipline-api-server}/bin/habitscipline-api-server
              '';
            serviceConfig =
              {
                Restart = "always";
                RestartSec = 1;
                Nice = 15;
              };
            unitConfig =
              {
                StartLimitIntervalSec = 0;
                # ensure Restart=always is always honoured
              };
          };
        };
      api-server-host =
        with cfg.api-server;

        optionalAttrs (enable && hosts != [ ]) {
          "${head hosts}" =
            {
              enableACME = true;
              forceSSL = true;
              locations."/" = {
                proxyPass = "http://localhost:${builtins.toString port}";
                # Just to make sure we don't run into 413 errors on big syncs
                extraConfig = ''
                  client_max_body_size 0;
                '';
              };
              serverAliases = tail hosts;
            };
        };

      # Local backup
      local-backup-service =
        optionalAttrs (cfg.api-server.enable or false) (
          optionalAttrs (cfg.api-server.local-backup.enable or false) (
            with cfg.api-server.local-backup;
            {
              "habitscipline-api-server-local-backup-${envname}" = {
                description = "Backup habitscipline-api-server database locally for ${envname}";
                wantedBy = [ ];
                script =
                  ''
                    mkdir -p ${backup-dir}
                    file="${backup-dir}/''$(date +%F_%T).db"
                    ${pkgs.sqlite}/bin/sqlite3 ${api-server-database-file} ".backup ''${file}"
                  '';
                serviceConfig = {
                  WorkingDirectory = working-dir;
                  Type = "oneshot";
                };
              };
            }
          )
        );
      local-backup-timer =
        optionalAttrs (cfg.api-server.enable or false) (
          optionalAttrs (cfg.api-server.local-backup.enable or false) (
            with cfg.api-server.local-backup;
            {
              "habitscipline-api-server-local-backup-${envname}" = {
                description = "Backup habitscipline-api-server database locally for ${envname} every twelve hours.";
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "00/12:00";
                  Persistent = true;
                };
              };
            }
          )
        );
    in
    mkIf cfg.enable {
      systemd.services =
        concatAttrs [
          api-server-service
          local-backup-service
        ];
      systemd.timers =
        concatAttrs [
          local-backup-timer
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional cfg.api-server.enable cfg.api-server.port)
      ];
      services.nginx.virtualHosts =
        concatAttrs [
          api-server-host
        ];
    };
}
