{
  ...
}:

{
  home.username = "justin";
  home.homeDirectory = "/home/justin";
  home.stateVersion = "26.05";

  # Pick up fonts
  fonts.fontconfig = {
    enable = true;
  };
  
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Allow unfree packages in home-manager packages
  nixpkgs.config.allowUnfree = true;

}