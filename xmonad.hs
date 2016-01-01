import XMonad
import qualified Data.Map as M -- for keys
import System.Exit

import XMonad.Layout.Grid
import XMonad.Hooks.ManageHelpers -- doFullFloat, isFullScreen etc
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive

main = xmonad $ ewmh defaultConfig
    { modMask = mod4Mask
    , terminal = "termite"
    , borderWidth = 0
    , layoutHook = minLayoutHook
    , manageHook = minManageHook
    , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
    , keys = minaKeys <+> keys defaultConfig
    , logHook = fadeInactiveCurrentWSLogHook 0.90 <+> -- fade inactive windows on active workspace
        dynamicLogWithPP xmobarPP {ppOrder = (\(ws:_:_:_) -> [ws]) } -- log in xmobar format, take only the workspace part (and notifications, where applicable)
    }

minLayoutHook = Tall 1 (1/40) (1/2) ||| Full ||| Grid

minaKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_d), spawn "dmenu_run")
    , ((modMask, xK_s), spawn "termite -t statusy -e htop")
    , ((modMask, xK_c), spawn "chromium")
    , ((modMask, xK_y), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in PATH: \"$PATH\"; fi")
    , ((modMask .|. shiftMask, xK_y), io (exitWith ExitSuccess)) -- Quit xmonad
    , ((modMask, xK_q), kill)
    , ((modMask .|. shiftMask, xK_q), kill) -- accidently quit otherwise
    , ((modMask, xK_F1), spawn "xbacklight = 4")
    , ((modMask, xK_F2), spawn "xbacklight = 16")
    , ((modMask, xK_F3), spawn "xbacklight = 50")
    , ((modMask, xK_F4), spawn "xbacklight = 100")
    ]

minManageHook = composeAll
    [ title =? "statusy" --> doFullFloat
    , isFullscreen --> doFullFloat
    , resource =? "stalonetray" --> doIgnore
    , title =? "album art" --> doSideFloat CE
    ]
