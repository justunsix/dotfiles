# Node 1 of a Kubernetes Cluster with k3s
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/networking/cluster/k3s/README.md
{
  pkgs,
  ...
}:
{
  networking.firewall.allowedTCPPorts = [
    6443 # k3s: required so that pods can reach the API server (running on port 6443 by default)
    # 2379 # k3s, etcd clients: required if using a "High Availability Embedded etcd" configuration
    # 2380 # k3s, etcd peers: required if using a "High Availability Embedded etcd" configuration
  ];
  networking.firewall.allowedUDPPorts = [
    # 8472 # k3s, flannel: required if using multi-node for inter-node networking
  ];
  services.k3s.enable = true;
  services.k3s.role = "server";
  services.k3s.extraFlags = toString [
    # "--debug" # Optionally add additional args to k3s
  ];

  # Multi-node Config, First node
  # services.k3s = {
  #   enable = true;
  #   role = "server";
  #   tokenFile = "/etc/nixos/local/k3s-token";
  ## or token = "<randomized common secret>";
  #   clusterInit = true;
  # };
}
