# JT's Dotfiles

Configuration and customization files to personalize Linux and Windows.

This repository contains my personal dotfiles like configurations and setups for the following:

- Operating systems/distributions:
  - GNU/Linux Ubuntu
  - GNU/Linux Fedora
  - Microsoft Windows
- Desktop environment: GNOME
- Window manager (WM): i3
  - Status bar: i3status
- Window switcher/launcher: rofi
- Shell:
  - Bash
  - Fish
  - Prompt: starship
- Clipboard manager: copyq
- Editor:
  - Emacs
  - Visual Studio Code
- Video player: mpv
- Notification daemon: dunst
- To do list: [todotxt](https://github.com/todotxt/todo.txt-cli)

## How Dotfiles are Managed

There are many ways to manage your dotfiles. I use Ansible inspired by [geerlingguy/mac-dev-playbook](https://github.com/geerlingguy/mac-dev-playbook) which also installs and configures machines I used from scratch using package managers like apt, dnf, deb-get, chocolately, and nix.
