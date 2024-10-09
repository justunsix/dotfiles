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
				-- Use Enter in insert mode to add list items/checkboxes/todos
				-- https://github.com/nvim-orgmode/orgmode/blob/nightly/DOCS.md#use-enter-in-insert-mode-to-add-list-itemscheckboxestodos
				mappings = {
					org_return_uses_meta_return = true,
				},
			})
		end,
	},
}
