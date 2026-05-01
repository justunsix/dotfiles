vim.api.nvim_create_user_command("JTCopyFilename", function()
  vim.cmd([[let @+ = expand('%:t')]])
end, { desc = "Copy current buffer filename to system clipboard" })
