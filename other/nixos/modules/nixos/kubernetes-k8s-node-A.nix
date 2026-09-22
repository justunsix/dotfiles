# Node 1 of a Kubernetes Cluster with k8s
# Manual: https://nixos.org/manual/nixos/stable/#sec-kubernetes
# Additional tips: https://wiki.nixos.org/wiki/Kubernetes
{
  pkgs,
  ...
}:
{
  services.kubernetes = {
    apiserver.enable = true;
    controllerManager.enable = true;
    scheduler.enable = true;
    addonManager.enable = true;
    proxy.enable = true;
    flannel.enable = true;
    # Assigning master and node roles for single node Kubernetes cluster for dev or testing 
    roles = ["master" "node"];
  };
}
