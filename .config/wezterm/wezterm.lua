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

-- On Windows, add PowerShell and pwsh to launcher, set default shell
if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
	 config.default_prog = { 'nu.exe' }
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
	 table.insert(config.launch_menu, { label = 'msys2-crt64', args = {'ucrt64.cmd'},
	 })
	 table.insert(config.launch_menu, { label = 'Cygwin Shell', args = {'cygwin-shell.bat'},
	 })


end

-- On Linux, add bash and nushell to launch, set default shell
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
	-- Turn off / Disable default alt-enter action 
  -- allowing it to be recognized and handled by the tab
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

-- Saving scrollback and pane
-- from: https://wezfurlong.org/wezterm/config/lua/pane/get_lines_as_text.html

local io = require 'io'
local os = require 'os'
local act = wezterm.action

wezterm.on('trigger-vim-with-scrollback', function(window, pane)
  -- Retrieve the text from the pane
  local text = pane:get_lines_as_text(pane:get_dimensions().scrollback_rows)

  -- Create a temporary file to pass to vim
  local name = os.tmpname()
  local f = io.open(name, 'w+')
  f:write(text)
  f:flush()
  f:close()

  -- Open a new window running vim and tell it to open the file
  -- window:perform_action(
  --   act.SpawnCommandInNewWindow {
  --     args = { 'vim', name },
  --   },
  --   pane
  -- )	

  -- output the filename's location to the current active pane
  pane:inject_output('Session saved to: ' .. name)

  -- Wait "enough" time for vim to read the file before we remove it.
  -- The window creation and process spawn are asynchronous wrt. running
  -- this script and are not awaitable, so we just pick a number.
  --
  -- Note: We don't strictly need to remove this file, but it is nice
  -- to avoid cluttering up the temporary directory.
  -- wezterm.sleep_ms(1000)
  -- os.remove(name)
end)

-- Merge the keys table with the existing one
table.insert(config.keys, {
  key = 'E',
  mods = 'CTRL',
  action = act.EmitEvent 'trigger-vim-with-scrollback',
})

-- ====================================================
-- and finally, return the configuration to wezterm
return config
