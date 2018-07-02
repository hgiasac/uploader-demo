{ config, lib, ... }:

with lib;

let
  cfg = config.services.fileAPI;
  pkgs = import ./release.nix { };
in
{

  options = {
    services.fileAPI = {
      enable = mkEnableOption "File Management REST API";

      uploadDirectory = mkOption {
        default = "/tmp/fileapi/uploads";
        type = types.str;
        description = "
          Upload file directory
        ";
      };

      dbPath = mkOption {
        default = "/tmp/fileapi/db";
        type = types.str;
        description = "
          SQLite database path
        ";
      };

      dbName = mkOption {
        default = "file.db";
        type = types.str;
        description = "
          SQLite database path
        ";
      };

      port = mkOption {
        default = 3000;
        type = types.int;
        description = "
          Service Port
        ";
      };
    };
  };


  config = mkIf cfg.enable {
    networking.hostName = "fileapi";
    networking.firewall.allowedTCPPorts = [ 22 80 cfg.port ];

    systemd.services.fileapi =
    { description = "File API Webserver";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      environment = {
        UPLOAD_DIR = "${cfg.uploadDirectory}";
        DATABASE_URL = "${cfg.dbPath}/${cfg.dbName}";
        PORT = toString cfg.port;
      };

      preStart = ''
        mkdir -p ${cfg.dbPath}
        mkdir -p ${cfg.uploadDirectory}
        chmod -R 1777 ${cfg.uploadDirectory}
        chmod -R 1777 ${cfg.dbPath}
      '';

      serviceConfig =
      { # User = "root";
        ExecStart = "${pkgs.fileapi}/bin/file-api";
        WorkingDirectory = "${pkgs.fileapi}";

      };
    };
  };

}
