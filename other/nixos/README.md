# Nix and NixOS Configuration - About this folder

Folder and nix modules follow
[Nix Starter Config by Misterio77](https://github.com/Misterio77/nix-starter-configs).

It builds on flake templates for getting started with NixOS + home-manager on
boot.

## Structure of Nix Configuration

### Main Files

- `flake.nix` - Entry point, define nixpkgs and other inputs for use in
  configurations
- `home-manager\` - home-manager configurations and modules
  - `home-non-nixos.nix` - home-manager configuration for other Linux
    distributions like Ubuntu, Fedora to exclude packages provided those in
    there base installations like the desktop environment
  - `home.nix` - home-manager configuration for NixOS systems
- `hosts\` - host specific configuration dependent on hardware like GPU,
  virtualization, gaming
  - Each host has a separate folder and `configuration.nix`

### Tree View

```text

├── flake.lock
├── flake.nix
├── home-manager
│   ├── home-non-nixos.nix
│   ├── home.nix
│   └── modules
│       ├── base.nix
│       ├── desktop.nix
│       ├── graphical-programs.nix
│       └── tools.nix
├── hosts
│   ├── nixosbtw
│   │   └── configuration.nix
│   ├── hostname1
│   │   └── configuration.nix
│   ├── hostname2
│   │   └── configuration.nix
│   ....
│
├── LICENSE.txt
└── README.md
```

## Usage

### NixOS System

```shell

# Rebuild host configuration
sudo nixos-rebuild switch --flake .#hostname
# or with nh
nh os switch . -H hostname

```

### User's configuration, programs with home-,anager

I use home-manager as a standalone installation on NixOS and other Linux
distributions and use `stow` for dotfiles management. Reasons why:

- I use Windows, MacOS, and Linux distributions other than NixOS. Those other
  systems can reuse the dotfiles with symlinks like `stow`
- MacOS and other Linux distributions can use the nix package manager with
  home-manager for program installation and configuration.
- Those systems can reuse the `flake.nix`'s home-manager configurations

```shell

# Rebuild home-manager configuration
home-manager switch --flake .#user@hostname
# or with nh
nh home siwtch . -c user@hostname

```

# References

- [Nix Starter Config by Misterio77](https://github.com/Misterio77/nix-starter-configs)
- [bashbunni/dotfiles](https://github.com/bashbunni/dotfiles/) - dotfiles that
  use a similar approach of Stow and NixOS and other Linux distributions
- [tonybanters/nixos-from-scratch](https://github.com/tonybanters/nixos-from-scratch) -
  installing NixOS using command line and creating a `flake.nix` and home
  manager set up
