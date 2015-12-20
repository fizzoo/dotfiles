import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Accordion
import qualified Data.Map as M

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog

main = xmonad defaultConfig
    { modMask = mod4Mask
    , terminal = "termite"
    , layoutHook = minLayoutHook
    , manageHook = minManageHook
    , keys = minaKeys <+> keys defaultConfig
    , logHook = dynamicLogWithPP xmobarPP {ppOrder = (\(ws:_:_:_) -> [ws]) }
    }

-- smartBorders
minLayoutHook = smartBorders $ Tall 1 (1/40) (1/2) ||| noBorders(Full) ||| Grid ||| Accordion

minaKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_d     ), spawn "dmenu_run")
    , ((modMask,               xK_s     ), spawn "termite -t statusy -e htop")
    , ((modMask,               xK_c     ), spawn "chromium")
    , ((modMask,               xK_y     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in PATH: \"$PATH\"; fi")
    , ((modMask,               xK_q     ), kill) -- %! Close the focused window
    , ((modMask,               xK_F1    ), spawn "xbacklight = 4")
    , ((modMask,               xK_F2    ), spawn "xbacklight = 16")
    , ((modMask,               xK_F3    ), spawn "xbacklight = 50")
    , ((modMask,               xK_F4    ), spawn "xbacklight = 100")
    ]

minManageHook = composeAll
    [ title =? "statusy" --> doFullFloat
    ]
