{
  fileapi =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 4096; # megabytes
      # deployment.virtualbox.vcpu = 2; # number of cpus
      deployment.virtualbox.headless = true;
      
    };
}
