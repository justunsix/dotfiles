{
  pkgs,
  ...
}:

{
  # GNOME Settings
  dconf = {
    settings = {
      # Dark mode for GTK apps
      "org/gnome/desktop/interface".color-scheme = "prefer-dark";
    };
  };

  # USB drive automounting
  # services.udiskie = {
  #   enable = true;
  #   settings = {
  #     # workaround for
  #     # https://github.com/nix-community/home-manager/issues/632
  #     program_options = {
  #       # replace with your favorite file manager
  #       file_manager = "${pkgs.nautilus}/bin/nautilus";
  #     };
  #   };
  # };

  # https://nix-community.github.io/home-manager/options/home-manager/services/gpg-agent.html#opt-services.gpg-agent.enable
  # https://tsawyer87.github.io/posts/gpg-agent_on_nixos/
  services.gpg-agent = {
    enable = true;
    # pinentry is a collection of simple PIN or passphrase dialogs used for
    # password entry
    pinentry.package = pkgs.pinentry-gnome3;
  };

  # Clipboard manager
  services.copyq.enable = true;
}
