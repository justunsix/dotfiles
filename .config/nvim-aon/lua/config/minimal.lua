-- ## Options

local set = vim.opt

-- Indentation
set.shiftwidth = 4
-- Use system clipbard
set.clipboard = "unnamedplus"

-- ## Keymaps, General

local km = vim.keymap

-- Source current file
km.set("n", "<space><space>x", "<cmd>source %<CR>")
-- Run selections
km.set("n", "<space>x", ":.lua<CR>")
km.set("v", "<space>x", ":lua<CR>")

-- Quickfix list movement
km.set("n", "<M-j>", "<cmd>cnext<CR>")
km.set("n", "<M-k>", "<cmd>cprev<CR>")
-- jk --> Escape
km.set("i", "jk", "<Esc>", { silent = true })

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
