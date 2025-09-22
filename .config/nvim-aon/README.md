# Advent of Neovim Inspired Configuration

Neovim configuration development started from
[Advent of Neovim by TJ DeVries on YouTube](https://www.youtube.com/watch?v=TQn2hJeHQbM&list=PLep05UYkc6wTyBe7kPjQFWVXTlhKeQejM&index=1)
with [supporting repository](https://github.com/tjdevries/advent-of-nvim/)

## Usage

```shell

# Run nvimexample configuration
NVIM_APPNAME=nvim-aon nvim

# Go to configuration directory to edit init.lua
# For Windows see Makefile
# This directory is for Linux
cd ~/.config/nvimexample
NVIM_APPNAME=nvim-aoe init.lua
```

See `Makefile` for setup and copying configuration files to Neovim example
configuration folder using `make install`.

Run `make install` after making Neovim configuration changes to file in this
repository.

Put plugins you want to use in the `lua/config/plugins` folder.

`after` lua files will be loaded after Neovim defaults. `after/ftplugin` sets
file type plugins that are loaded for certain file types.
