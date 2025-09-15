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
				-- for folds on opening file, inherit from nvim's global foldlevel
				org_startup_folded = "inherit",
				-- Indents are only visual and not saved in file
				org_startup_indented = true,
				org_adapt_indentation = false,
				-- Enter in insert mode will always do org meta return
				-- per fix in above comments
				-- mappings = {
				--  org_return_uses_meta_return = true,
				-- },
			})
		end,
	},
}
