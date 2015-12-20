import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Accordion
import qualified Data.Map as M
import Data.Bits ((.|.))
import XMonad.Hooks.ManageHelpers

main = xmonad defaultConfig
    { modMask = mod4Mask
    , terminal = "termite"
    , layoutHook = minLayoutHook
    , manageHook = minManageHook
    , keys = minaKeys <+> keys defaultConfig
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
    ]

minManageHook = composeAll
    [ title =? "statusy" --> doFullFloat
    ]
