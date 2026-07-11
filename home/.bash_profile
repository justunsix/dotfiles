# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# Ubuntu Gnome reads from .bash_profile on start up
# Variables available for GUI applications are declared here

# QT_QPA_PLATFORMTHEME sets theme for QT apps so Dolphin
# will use the correct them for example set qt5ct
export QT_QPA_PLATFORMTHEME=qt5ct

# For Qt applications, use Kvantum
# per:
# - https://wiki.archlinux.org/title/Uniform_look_for_Qt_and_GTK_applications#GTK_themes_ported_to_Kvantum
# - https://itsfoss.com/vlc-dark-mode/
# export QT_STYLE_OVERRIDE=kvantum
