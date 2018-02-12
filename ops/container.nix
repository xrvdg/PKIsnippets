{config, pkgs, ...}:
{
    networking.firewall.allowedTCPPorts = [8200 7474 7473];
    services.neo4j =
    {
      enable = true;
      listenAddress = "db";
      enableHttps = true;
    };

    systemd.services.pki = let pki = pkgs.callPackage ../default.nix {}; in {
      enable = true;
      description = "PKI service";
      environment.ADDR = "db";
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pki}/bin/PKI";
      };
      wantedBy = ["multi-user.target"];
    };
}

