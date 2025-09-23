print("Advent of Neovim inspired configuration")
-- Inspired by https://github.com/tjdevries/advent-of-nvim/

-- Minimal configuration of Neovim defaults
require("config.minimal")

-- ## Terminal
require("config.terminal")

-- ## Plugins

-- ## Plugins management
require("config.lazy")

-- ## File Navigation
require("oil").setup()

-- ## Completion
require("blink.cmp").setup()

-- ## nvim-mini setup and modules including: status line
-- Set up 'mini.deps' (customize to your liking)
require("mini.deps").setup()

-- Use 'mini.deps'. `now()` and `later()` are helpers for a safe two-stage
-- startup and are optional.
local add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later

-- Safely execute immediately
now(function()
  vim.o.termguicolors = true
end)
now(function()
  require("mini.statusline").setup()
end)
-- ### End nvim-mini setup and mini.nvim modules
