-- Map Shift + Enter to org meta return
-- https://github.com/nvim-orgmode/orgmode/blob/nightly/DOCS.md#use-enter-in-insert-mode-to-add-list-itemscheckboxestodos
-- vim.api.nvim_create_autocmd("FileType", {
-- 	pattern = "org",
-- 	callback = function()
-- 		vim.keymap.set("i", "<S-CR>", '<cmd>lua require("orgmode").action("org_mappings.meta_return")<CR>', {
-- 			silent = true,
-- 			buffer = true,
-- 		})
-- 	end,
-- })
--
return {

	{
		"nvim-orgmode/orgmode",
		event = "VeryLazy",
		ft = { "org" },
		config = function()
			-- Setup orgmode
			require("orgmode").setup({
				org_agenda_files = "~/orgfiles/**/*",
				org_default_notes_file = "~/orgfiles/refile.org",
				-- Enter in insert mode will always do org meta return
				-- per fix in above comments
				mappings = {
					org_return_uses_meta_return = true,
				},
			})
		end,
	},
}
