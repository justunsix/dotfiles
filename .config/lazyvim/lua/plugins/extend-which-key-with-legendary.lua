return {
  "which-key.nvim",
  dependencies = {
    {
      "mrjones2014/legendary.nvim",
      -- since legendary.nvim handles all your keymaps/commands,
      -- its recommended to load legendary.nvim before other plugins
      -- config adapted from https://github.com/Martin1887/dotfiles/blob/main/stow/nvim/.config/nvim/lua/plugins/misc.lua
      priority = 10000,
      lazy = false,
      keys = {
        { "<leader>P", "<cmd>Legendary<cr>", desc = "Legendary Command Palette" },
      },
      opts = {
        extensions = {
          lazy_nvim = true,
          --          lazy_nvim = {
          --          -- Automatically register keymaps that are defined on lazy.nvim plugin specs
          --        -- using the `keys = {}` property.
          --      auto_register = true,
          --    },
          which_key = {
            -- Automatically add which-key tables to legendary
            -- see WHICH_KEY.md for more details
            auto_register = true,
          },
        },
        -- sqlite is only needed if you want to use frecency sorting
        -- dependencies = { 'kkharji/sqlite.lua' }
      },
    },
  },
}
