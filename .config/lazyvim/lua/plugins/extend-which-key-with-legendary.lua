return {
  "which-key.nvim",
  dependencies = {
    {
      "mrjones2014/legendary.nvim",
      -- version = 'v2.13.9',
      -- since legendary.nvim handles all your keymaps/commands,
      -- its recommended to load legendary.nvim before other plugins
      priority = 10000,
      lazy = false,
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
      },
    },
  },
}
