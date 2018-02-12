{
  network.description = "neo4j";
  db = {config, pkgs, ...}:
  {
    imports = [./container.nix];
    deployment.targetEnv = "container";
    deployment.container =
    {
      forwardPorts = [{hostPort = 8200;} {hostPort = 7473;} {hostPort = 7474;}];
    };

  };
}
