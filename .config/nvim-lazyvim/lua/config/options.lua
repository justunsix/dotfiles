-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- Enable line wrapping
vim.opt.wrap = true
-- Open all folds by default
vim.opt.foldlevelstart = 99
-- Neovim Shared Data (shada) and history
-- '1000: Save marks for 1000 files
-- <50: Save up to 50 lines for each register
-- /100: Save the last 100 search patterns.
-- s1000: Save up to 1000 Kib for each item
vim.opt.shada = "'1000,/100,<50,s1000"

-- Extras
-- 
-- lang.python
-- https://www.lazyvim.org/extras/lang/python
-- LSP Server to use for Python.
-- Use ty instead of basedpyright or pyright and install ty in OS or Mason
vim.g.lazyvim_python_lsp = "ty"
