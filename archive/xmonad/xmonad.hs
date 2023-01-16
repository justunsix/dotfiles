-- Template file from https://wiki.haskell.org/Xmonad/Config_archive
-- Tutorial and samples: https://xmonad.org/TUTORIAL.html

------------------------------------------------------------------------
-- Imports

-- Base
import XMonad

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
-- Unavilable imports in xmonad < 0.17.0
-- Disabled for Ubuntu repo compatibility
-- import XMonad.Hooks.StatusBar
-- import XMonad.Hooks.StatusBar.PP

-- Layouts
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier

-- Utilities
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
  
-- ColorScheme
-- import Colors.DoomOne

------------------------------------------------------------------------
-- Variables

-- Sets modkey to super/windows key
myModMask :: KeyMask
myModMask = mod4Mask

-- Sets default terminal
myTerminal :: String
myTerminal = "gnome-terminal"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = "emacs"  -- Sets emacs as editor
-- myEditor = "vim "    -- Sets vim as editor

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook = do
        -- Set keyboard to US
        spawnOnce "setxkbmap us"

------------------------------------------------------------------------
-- Layout
myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 (ThreeColMid nmaster delta ratio)
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

-- https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-DynamicLog.html#t:PP
-- Removed lines due to logTitles and xmobarBorder unavailable xmonad version less than 0.17.0
--    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent = wrap (blue "[") (blue "]")  
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

------------------------------------------------------------------------
-- Main

-- Load default config and custom variables
-- See additional keys at https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html
-- Ensure Windows are Extended Window Manager Hints (EWMH) compliant
main :: IO ()
main = xmonad
-- Unavilable in xmonad < 0.17.0
-- Disabled for Ubuntu repo compatibility    
--   . ewmhFullscreen
     . ewmh 
   =<< statusBar "xmobar ~/.config/xmobar/xmobarrc" def toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)
     -- default toggle bar key is M-b
     -- . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     -- myConfig
  -- Launch xmobar on first monitor (0)
  -- xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"

myConfig = def
  { layoutHook                  = myLayout
    , modMask                   = myModMask
    , terminal                  = myTerminal
    , startupHook               = myStartupHook
  }
 `additionalKeysP`
    [ ("M-l", spawn "xscreensaver-command -lock")
    , ("<Print>", unGrab *> spawn "scrot -s"        )
    , ("M-f"  , spawn "firefox"                     )
    , ("M-e"  , spawn "emacs"                       )
    ]
