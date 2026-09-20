{
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [

    # ------------------------------------------------------------
    # WINE - compatibility layer capable of running Windows applications
    protontricks

  ];

}
