{
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [

    # ------------------------------------------------------------
    # Linux Game Programs

    # General
    lutris

    # GOG, Epic, Amazon
    heroic

  ];
}
