import qualified Data.Map                   as M (Map, fromList)
import           Data.Maybe                 (fromMaybe)
import           System.Exit                (exitSuccess)
import           XMonad
import qualified XMonad.StackSet            as W

import           Control.Monad              (when)
import           Data.List                  (isInfixOf)
import           Network.HostName           (getHostName)
import           XMonad.Hooks.DynamicLog    (dynamicLogWithPP, ppOrder,
                                             ppOutput, xmobarPP)
import           XMonad.Hooks.EwmhDesktops  (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks   (avoidStruts, docksEventHook,
                                             manageDocks)
import           XMonad.Hooks.ManageHelpers (doFullFloat)
import           XMonad.Layout.GridVariants (Grid (..))
import           XMonad.Layout.LayoutHints  (layoutHintsWithPlacement)
import           XMonad.Layout.Spacing      (spacing)
import           XMonad.Util.Run            (hPutStrLn, spawnPipe)

main :: IO ()
main = do
  bar <- spawnPipe "xmobar"

  xmonad $ ewmh def
    { modMask = mod4Mask
    , terminal = "termite"
    , borderWidth = 0
    , layoutHook = avoidStruts (layoutHintsWithPlacement (0.5, 0.5) (spacing 4 $ Grid (16/9))) ||| Full
    , manageHook = myManageHook <+> manageDocks
    , handleEventHook = handleEventHook def <+> fullscreenEventHook <+> docksEventHook
    , keys = myKeys
    , mouseBindings = myMouseBindings
    , logHook = dynamicLogWithPP xmobarPP {ppOrder = \(ws:_:wt:_) -> [ws, wt], ppOutput = hPutStrLn bar }
    , startupHook = myStartup
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
  , ((mod4Mask, xK_d), spawn "dmenu_run -b -i -nf \"#888888\" -nb \"#2D1F21\" -sf \"#ffffff\" -sb \"#6D1F21\" -fn \"Dina-13\" -l 12")
  , ((mod4Mask, xK_s), spawn "termite -t fully -e htop")
  , ((mod4Mask, xK_x), spawn "emacs")
  , ((mod4Mask, xK_w), spawn "termite -t fully -e statusy")
  , ((mod4Mask, xK_c), spawn "chrome")
  , ((mod4Mask, xK_o), spawn "swapsinks")
  , ((mod4Mask, xK_p), spawn "pavucontrol")
  , ((0, xK_Print), spawn "maim -s")
  , ((0, 0x1008ff45), spawn "maim -s") -- F14
  , ((mod4Mask, xK_Delete), spawn "susp")
  , ((0, 0x1008ff49), spawn "sleep 0.1 && xset dpms force off") -- F18
  , ((mod4Mask, xK_y), spawn "xmonad --recompile && xmonad --restart")
  , ((mod4Mask .|. shiftMask, xK_y), io exitSuccess)
  , ((mod4Mask, xK_q), kill)
  , ((mod4Mask, xK_F1), spawn "xbacklight = 4")
  , ((mod4Mask, xK_F2), spawn "xbacklight = 16")
  , ((mod4Mask, xK_F3), spawn "xbacklight = 50")
  , ((mod4Mask, xK_F4), spawn "xbacklight = 100")
  , ((mod4Mask, xK_t), sinkAll >> setLayout (XMonad.layoutHook conf))
  , ((mod4Mask, xK_a), sinkAll >> floatAllCurrent)
  , ((mod4Mask, xK_f), sendMessage NextLayout)
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
  ]

myManageHook :: ManageHook
myManageHook = composeAll
  [ title =? "fully" --> doFullFloat
  , title =? "WAKEUP" --> (doShiftScreen 2 <+> doFullFloat)
  , title =? "stalonetray" --> doShiftScreen 2
  , fmap ("qBittorrent" `isInfixOf`) title --> doShift "8"
  ]

myStartup :: X ()
myStartup = do
  host <- io getHostName
  spawn "trayer-srg --widthtype request --height 20 --transparent true --tint 0x00000000 --alpha 0"
  when (host == "mag") $ spawn "qbittorrent"

doShiftScreen :: ScreenId -> ManageHook
doShiftScreen n = liftX (screenWorkspace n) >>= doShift . fromMaybe "0"

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
floaty = fmap snd . floatLocation

getWindows :: X [Window]
getWindows = withWindowSet (return . W.integrate' . W.stack . W.workspace . W.current)
