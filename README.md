# JT's Dotfiles

Configuration and customization files to personalize Linux and Windows.

This repository contains my personal dotfiles like configurations and setups for the following:

- Operating systems/distributions:
  - [GNU/Linux Ubuntu using GNOME](https://ubuntu.com/desktop)
  - [GNU/Linux Fedora using i3 spin](https://spins.fedoraproject.org/en/i3/)
  - [Microsoft Windows](https://www.microsoft.com/en-ca/windows)
- Desktop environment: [GNOME](https://www.gnome.org/)
- Window manager (WM): [i3 tiling WM](https://i3wm.org/)
  - Status bar: [i3status](https://i3wm.org/docs/i3status.html)
- Window switcher/application launcher: [rofi](https://github.com/davatorium/rofi)
- Shell:
  - [Bash](https://www.gnu.org/software/bash/)
  - [Fish](https://fishshell.com/)
  - Prompt: [Starship](https://starship.rs/)
- Clipboard manager: [copyq](https://hluk.github.io/CopyQ/)
- Editor:
  - [Emacs](https://www.gnu.org/software/emacs/)
  - [Visual Studio Code](https://code.visualstudio.com/)
- Video player: [mpv](https://mpv.io/)
- Notification daemon: [dunst](https://dunst-project.org/)
- Task management: [todotxt](https://github.com/todotxt/todo.txt-cli)

## How Dotfiles are Managed

There are many ways to manage your dotfiles. I use Ansible inspired by [geerlingguy/mac-dev-playbook](https://github.com/geerlingguy/mac-dev-playbook) which also installs and configures machines I use from initial installation using package managers like apt, dnf, deb-get, chocolately, and nix.
