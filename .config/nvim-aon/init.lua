print("Advent of Neovim inspired configuration")
-- Inspired by https://github.com/tjdevries/advent-of-nvim/

-- ## Neovim options

-- Indentation
vim.opt.shiftwidth = 4
-- Use system clipbard
vim.opt.clipboard = "unnamedplus"

-- ## Keymaps, General

-- Source current file
vim.keymap.set("n", "<space><space>x", "<cmd>source %<CR>")
-- Run selections
vim.keymap.set("n", "<space>x", ":.lua<CR>")
vim.keymap.set("v", "<space>x", ":lua<CR>")

-- Quickfix list movement
vim.keymap.set("n", "<M-j>", "<cmd>cnext<CR>")
vim.keymap.set("n", "<M-k>", "<cmd>cprev<CR>")
-- jk --> Escape
vim.keymap.set("i", "jk", "<Esc>", { silent = true })

-- ## Autocommands

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

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
