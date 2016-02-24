import XMonad
import qualified Data.Map as M -- for keys
import System.Exit

import XMonad.Hooks.ManageHelpers -- doFullFloat, isFullScreen etc
import XMonad.Hooks.DynamicLog -- for xmobar
import XMonad.Hooks.EwmhDesktops -- fullscreenEventHook, emwh etc
import XMonad.Hooks.ManageDocks -- avoidStruts, manageDocks, docksEventHook etc
import XMonad.Layout.GridVariants --grid variant that tiles the first two vertically

main :: IO ()
main = xmonad $ ewmh def
    { modMask = mod4Mask
    , terminal = "termite"
    , borderWidth = 0
    , layoutHook = minLayoutHook
    , manageHook = minManageHook <+> manageDocks
    , handleEventHook = handleEventHook def <+> fullscreenEventHook <+> docksEventHook
    , keys = minaKeys <+> keys def
    , logHook = dynamicLogWithPP xmobarPP {ppOrder = \(ws:_) -> [ws] } -- log in xmobar format, take only the workspace part (and notifications, where applicable)
    }

minLayoutHook = grid ||| Full
    where
    grid = Grid (16/9) -- first 3 windows are exactly like tall, then degrades gracefully into grids

minaKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
minaKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_d), spawn "dmenu_run")
    , ((modMask, xK_s), spawn "termite -t statusy -e htop")
    , ((modMask, xK_c), spawn "chromium")
    , ((modMask, xK_p), spawn "pavucontrol")
    , ((modMask, xK_i), spawn "maim -s")
    , ((modMask .|. shiftMask, xK_i), spawn "maim -s")
    , ((modMask, xK_y), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in PATH: \"$PATH\"; fi")
    , ((modMask .|. shiftMask, xK_y), io exitSuccess) -- Quit xmonad
    , ((modMask, xK_q), kill)
    , ((modMask .|. shiftMask, xK_q), kill) -- accidently quit otherwise
    , ((modMask, xK_F1), spawn "xbacklight = 4")
    , ((modMask, xK_F2), spawn "xbacklight = 16")
    , ((modMask, xK_F3), spawn "xbacklight = 50")
    , ((modMask, xK_F4), spawn "xbacklight = 100")
    ]

minManageHook :: ManageHook
minManageHook = composeAll
    [ title =? "statusy" --> doFullFloat
    , isFullscreen --> doFullFloat
    , title =? "album art" --> doSideFloat CE
    ]
