return {
	{ "nvchad/volt", lazy = true },
	{ "nvchad/menu", lazy = true },
	-- mouse users + nvimtree users!
	vim.keymap.set("n", "<RightMouse>", function()
		vim.cmd.exec('"normal! \\<RightMouse>"')
		local opts = { mouse = true, border = false }
		local options = vim.bo.ft == "NvimTree" and "nvimtree" or "default"
		require("menu").open(options, opts)
	end, {}),
}
