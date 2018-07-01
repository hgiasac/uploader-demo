let
  region = "ap-northeast-1";
  accessKeyId = "xxx";

in
{ fileapi =
  { resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    deployment.ec2.ebsInitialRootDiskSize = 20;
    deployment.ec2.instanceType ="t2.medium";
    deployment.ec2.keyPair = resources.ec2KeyPairs.fileapi-keys;

    nixpkgs.system = "x86_64-linux";
  };

  resources.ec2KeyPairs.fileapi-keys =
    { inherit region accessKeyId; };
}
