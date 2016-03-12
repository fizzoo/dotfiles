import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.Exit

import XMonad.Hooks.ManageHelpers -- doFullFloat, isFullScreen etc
import XMonad.Hooks.EwmhDesktops -- fullscreenEventHook, emwh etc
import XMonad.Hooks.ManageDocks -- avoidStruts, manageDocks, docksEventHook etc
import XMonad.Layout.GridVariants -- grid variant that tiles the first two vertically
import XMonad.Actions.WithAll

main :: IO ()
main = xmonad $ ewmh def
    { modMask = mod4Mask
    , terminal = "termite"
    , borderWidth = 0
    , layoutHook = Grid (16/9) ||| Full
    , manageHook = minManageHook <+> manageDocks
    , handleEventHook = handleEventHook def <+> fullscreenEventHook <+> docksEventHook
    , keys = minaKeys
    }

minaKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
minaKeys conf = M.fromList $
    [ ((mod4Mask, xK_Return), spawn $ XMonad.terminal conf)
    , ((mod4Mask, xK_space), sendMessage NextLayout)
    , ((mod4Mask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((mod4Mask, xK_Tab), windows W.focusDown)
    , ((mod4Mask .|. shiftMask, xK_Tab), windows W.focusUp)
    , ((mod4Mask, xK_j), windows W.focusDown)
    , ((mod4Mask, xK_k), windows W.focusUp)
    , ((mod4Mask .|. shiftMask, xK_j), windows W.swapDown)
    , ((mod4Mask .|. shiftMask, xK_k), windows W.swapUp)
    , ((mod4Mask, xK_t), withFocused $ windows . W.sink)
    , ((mod4Mask, xK_d), spawn "dmenu_extended_run")
    , ((mod4Mask, xK_s), spawn "termite -t statusy -e htop")
    , ((mod4Mask, xK_c), spawn "chromium")
    , ((mod4Mask, xK_p), spawn "pavucontrol")
    , ((mod4Mask, xK_i), spawn "maim -s")
    , ((mod4Mask, xK_y), spawn "xmonad --recompile && xmonad --restart")
    , ((mod4Mask .|. shiftMask, xK_y), io exitSuccess)
    , ((mod4Mask, xK_q), kill)
    , ((mod4Mask, xK_F1), spawn "xbacklight = 4")
    , ((mod4Mask, xK_F2), spawn "xbacklight = 16")
    , ((mod4Mask, xK_F3), spawn "xbacklight = 50")
    , ((mod4Mask, xK_F4), spawn "xbacklight = 100")
    , ((mod4Mask, xK_f), sinkAll)
    ]
    ++
    [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

minManageHook :: ManageHook
minManageHook = composeAll
    [ title =? "statusy" --> doFullFloat
    , isFullscreen --> doFullFloat
    , title =? "album art" --> doSideFloat CE
    ]
