import XMonad
import qualified Data.Map as M (fromList, Map)
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)
import Control.Monad (liftM)

import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.ManageDocks (manageDocks, docksEventHook)
import XMonad.Layout.GridVariants (Grid(..))
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Layout.Spacing (spacing)

main :: IO ()
main = xmonad $ ewmh def
  { modMask = mod4Mask
  , terminal = "xterm"
  , borderWidth = 0
  , layoutHook = spacing 4 $ Grid (16/9)
  , manageHook = myManageHook <+> manageDocks
  , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook
  , keys = myKeys
  , mouseBindings = myMouseBindings
  }

-- Some keys brought straight from default config in order to have one overview
-- of all active bindings.
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
  [ ((mod4Mask, xK_Return), spawn $ XMonad.terminal conf)
  , ((mod4Mask, xK_Tab), windows W.focusDown)
  , ((mod4Mask .|. shiftMask, xK_Tab), windows W.focusUp)
  , ((mod4Mask, xK_j), windows W.focusDown)
  , ((mod4Mask, xK_k), windows W.focusUp)
  , ((mod4Mask .|. shiftMask, xK_j), windows W.swapDown)
  , ((mod4Mask .|. shiftMask, xK_k), windows W.swapUp)
  , ((mod4Mask, xK_d), spawn "dmenu_extended_run")
  , ((mod4Mask, xK_s), spawn "xterm -T fully -e htop")
  , ((mod4Mask, xK_w), spawn "xterm -T fully -e statusy")
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
  , ((mod4Mask, xK_t), sinkAll)
  , ((mod4Mask, xK_a), sinkAll >> floatAllCurrent)
  , ((mod4Mask, xK_f), withFocused $ windows . (\w -> W.float w (W.RationalRect 0 0 1 1)))
  ]
  ++
  [((m .|. mod4Mask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings _ = M.fromList
  [ ((mod4Mask, button1), \w -> focus w >> mouseMoveWindow w
                                        >> windows W.shiftMaster)
  , ((mod4Mask, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((mod4Mask, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
  , ((mod4Mask, button4), \w -> focus w >> windows (W.sink w))
  , ((mod4Mask, button5), \w -> focus w >> windows (W.float w (W.RationalRect 0 0 1 1)))
  ]

myManageHook :: ManageHook
myManageHook = composeAll
  [ title =? "fully" --> doFullFloat
  ]


-- Floats all windows, while keeping them in the positions they were in (unlike
-- a simple sequence float where it calculates new positions after each float
-- and they end up on top of eachother).
floatAllCurrent :: X ()
floatAllCurrent = do 
  ws <- getWindows
  wr <- mapM floaty ws
  mapM_ (windows . uncurry W.float) (zip ws wr)

sinkAll :: X ()
sinkAll = do
  ws <- getWindows
  mapM_ (windows . W.sink) ws

floaty :: Window -> X W.RationalRect
floaty = liftM snd . floatLocation

getWindows :: X [Window]
getWindows = withWindowSet (return . W.integrate' . W.stack . W.workspace . W.current)
