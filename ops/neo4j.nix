{
  network.description = "neo4j";
  db = {config, pkgs, ...}:
  let addr = "10.233.1.2";
  in
  {
    deployment.targetEnv = "container";
    deployment.container =
    {
      hostAddress = "10.233.1.1";
      localAddress = addr;
      forwardPorts = [{hostPort = 7473; containerPort = 7473;} {hostPort = 7474; containerPort = 7474;}];
    };
    networking.firewall.allowedTCPPorts = [7474 7473];
    services.neo4j =
    { enable = true;
      listenAddress = addr;
      enableHttps = true;
    };
    environment.systemPackages = let pki = pkgs.callPackage ../default.nix {}; in [pki];
  };
}
