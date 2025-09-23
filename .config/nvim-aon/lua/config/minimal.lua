-- ## Options

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
