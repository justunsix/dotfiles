-- File template from
-- https://wezfurlong.org/wezterm/config/files.html#configuration-file-structure

-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
-- config.color_scheme = 'AdventureTime'

-- Enable the scrollbar.
-- It will occupy the right window padding space.
-- If right padding is set to 0 then it will be increased
-- to a single cell width
config.enable_scroll_bar = true

-- and finally, return the configuration to wezterm
return config
