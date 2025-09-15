return {

	{
		"nvim-orgmode/orgmode",
		event = "VeryLazy",
		ft = { "org" },
		config = function()
			-- Setup orgmode
			require("orgmode").setup({
				-- For folds on opening file, inherit from nvim's global foldlevel
				org_startup_folded = "inherit",
				-- Indents are only visual and not saved in file
				org_startup_indented = true,
				org_adapt_indentation = false,
				-- Map Alt + Enter to org meta return
				-- https://github.com/nvim-orgmode/orgmode/blob/master/docs/
				-- configuration.org#use-enter-in-insert-mode-to-add-list-itemscheckboxestodos
				vim.api.nvim_create_autocmd("FileType", {
					pattern = "org",
					callback = function()
						vim.keymap.set(
							"i",
							"<M-CR>",
							'<cmd>lua require("orgmode").action("org_mappings.meta_return")<CR>',
							{
								silent = true,
								buffer = true,
							}
						)
					end,
				}),
			})
		end,
	},
}
