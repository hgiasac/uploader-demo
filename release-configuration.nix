{
  network.description = "File Management REST API";

  fileapi =
    { config, pkgs, ... }: let
    fileapi = import ./release.nix { inherit pkgs; };
  in
  {

    networking.hostName = "filepi";

    networking.firewall.allowedTCPPorts = [ 22 80 3000 ];
    environment.systemPackages = [ fileapi ];

    systemd.services.fileapi =
    { description = "File API Webserver";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      environment = {
        UPLOAD_DIR = "/tmp/fileapi/uploads";
        DATABASE_URL = "/tmp/fileapi/db/file.db";
        PORT = "3000";
      };

      preStart = ''
        mkdir -p /tmp/fileapi/db
        mkdir -p /tmp/fileapi/uploads
        chmod -R 1777 /tmp/fileapi/db
        chmod -R 1777 /tmp/fileapi/uploads
      '';

      serviceConfig =
      { # User = "root";
        ExecStart = "${fileapi}/bin/file-api";
        WorkingDirectory = "${fileapi}";

      };
    };
  };
}
