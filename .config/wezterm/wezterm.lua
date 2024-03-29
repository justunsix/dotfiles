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

-- ====================================================
-- ====================================================
-- This is where you actually apply your config choices

-- For example, changing the color scheme:
-- config.color_scheme = 'AdventureTime'

-- On Windows, add PowerShell and pwsh to launcher, set powershell as default shell
if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
	 config.default_prog = { 'pwsh.exe' }
	 config.launch_menu = {}
	 table.insert(config.launch_menu, { label = 'PowerShell -NoProfile', args = {'powershell.exe', '-NoProfile', '-NoLogo'},
	 })
	 table.insert(config.launch_menu, { label = 'PowerShell', args = {'powershell.exe', '-NoLogo'},
	 })
   table.insert(config.launch_menu, { label = 'pwsh -NoProfile', args = {'pwsh.exe', '-NoProfile'},
	 })
	 table.insert(config.launch_menu, { label = 'pwsh', args = {'pwsh.exe'},
	 })
	 table.insert(config.launch_menu, { label = 'nushell', args = {'nu.exe'},
	 })
	 table.insert(config.launch_menu, { label = 'cmd', args = {'cmd.exe'},
	 })


end

-- On Linux, add bash and nushell to launch, set bash as default shell
if wezterm.target_triple == 'x86_64-unknown-linux-gnu' then
	 config.default_prog = { 'bash' }
	 config.launch_menu = {}
	 table.insert(config.launch_menu, { label = 'bash', args = {'bash'},
	 })
	 table.insert(config.launch_menu, { label = 'nushell', args = {'nu'},
	 })
end

-- Enable the scrollbar.
-- It will occupy the right window padding space.
-- If right padding is set to 0 then it will be increased
-- to a single cell width
config.enable_scroll_bar = true

config.keys = {
	-- Override ToogleFullScreen keybinding to F11
	-- Turn off the default alt-enter action, allowing it to
  -- be potentially recognized and handled by the tab
  {
    key = 'Enter',
    mods = 'ALT',
    action = wezterm.action.DisableDefaultAssignment,
  },		
	{
		key = "F11",
    action = wezterm.action.ToggleFullScreen,
  },

}


-- ====================================================
-- and finally, return the configuration to wezterm
return config
