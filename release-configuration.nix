{
  network.description = "File Management REST API";

  fileapi =
    { config, pkgs, ... }: let
      fileapi = import ./release.nix { };
      uploadDirectory = "/tmp/fileapi/uploads";
      dbPath = "/tmp/fileapi/db";
      dbName = "files.db";
      port = "3000";
  in
  {

    networking.hostName = "filepi";

    networking.firewall.allowedTCPPorts = [ 22 80 3000 ];
    # environment.systemPackages = [ fileapi ];

    systemd.services.fileapi =
    { description = "File API Webserver";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      environment = {
        UPLOAD_DIR = uploadDirectory;
        DATABASE_URL = "${dbPath}/${dbName}";
        PORT = port;
      };

      preStart = ''
        mkdir -p ${dbPath}
        mkdir -p ${uploadDirectory}
        chmod -R 1777 ${dbPath}
        chmod -R 1777 ${uploadDirectory}
      '';

      serviceConfig =
      { # User = "root";
        ExecStart = "${fileapi.fileapi}/bin/file-api";
        WorkingDirectory = "${fileapi.fileapi}";

      };
    };
  };
}
