import           Control.Monad                    (when)
import           XMonad.Util.Run                  (runProcessWithInput)
import           Data.List                        (isInfixOf)
import qualified Data.Map                         as M (Map, fromList)
import           Data.Maybe                       (fromMaybe)
import           System.Environment               (getEnv)
import           System.Exit                      (exitSuccess)
import           XMonad
import qualified XMonad.Actions.FlexibleResize    as Flex
import           XMonad.Actions.Warp              (warpToWindow)
import           XMonad.Hooks.DynamicLog          (dynamicLogWithPP, ppOrder,
                                                   ppOutput, xmobarPP)
import           XMonad.Hooks.EwmhDesktops        (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks         (avoidStruts, docksEventHook,
                                                   manageDocks)
import           XMonad.Hooks.ManageHelpers       (doFullFloat)
import           XMonad.Layout.GridVariants       (Grid (..))
import           XMonad.Layout.LayoutHints        (layoutHintsWithPlacement)
import           XMonad.Layout.MouseResizableTile
import qualified XMonad.StackSet                  as W
import           XMonad.Util.Run                  (hPutStrLn, spawnPipe)

main :: IO ()
main = do
  bar <- spawnPipe "xmobar"

  xmonad $ ewmh def
    { modMask = mod4Mask
    , terminal = myterm
    , borderWidth = 0
    , layoutHook = avoidStruts (layoutHintsWithPlacement (0.5, 0.5) (mouseResizableTile { draggerType = FixedDragger 4 4 })) ||| Full
    , manageHook = myManageHook <+> manageDocks
    , handleEventHook = handleEventHook def <+> fullscreenEventHook <+> docksEventHook
    , keys = myKeys
    , mouseBindings = myMouseBindings
    , logHook = dynamicLogWithPP xmobarPP {ppOrder = \(ws:_:wt:_) -> [ws, wt], ppOutput = hPutStrLn bar }
    , startupHook = myStartup
    }

myterm = "st"

-- Some keys brought straight from default config in order to have one overview
-- of all active bindings.
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
  [ ((m,  xK_Return),  spawn $ XMonad.terminal conf)
  , ((ms, xK_Return),  spawn "xterm")
  , ((m,  xK_Tab),     down)
  , ((ms, xK_Tab),     up)
  , ((m,  xK_j),       down)
  , ((m,  xK_k),       up)
  , ((ms, xK_j),       swapdown)
  , ((ms, xK_k),       swapup)
  , ((m,  xK_d),       spawn "dmenu_run -b -i -nf \"#888888\" -nb \"#2D1F21\" -sf \"#ffffff\" -sb \"#6D1F21\" -fn \"Dina-10\" -l 12")
  , ((m,  xK_s),       spawn $ myterm ++ " -t fully -e htop")
  , ((m,  xK_x),       spawn "emacs")
  , ((m,  xK_c),       spawn "firefox")
  , ((m,  xK_o),       spawn "swapsinks")
  , ((m,  xK_p),       spawn "pavucontrol")
  , ((0,  xK_Print),   maim)
  , ((0,  0x1008ff45), maim) -- F14
  , ((m,  xK_Delete),  spawn "susp")
  , ((0,  0x1008ff49), spawn "sleep 0.1 && xset dpms force off") -- F18
  , ((m,  xK_y),       spawn "xmonad --recompile && xmonad --restart")
  , ((ms, xK_y),       io exitSuccess)
  , ((m,  xK_q),       kill)
  , ((m,  xK_F1),      spawn "sudo backlight 64")
  , ((m,  xK_F2),      spawn "sudo backlight 200")
  , ((m,  xK_F3),      spawn "sudo backlight 400")
  , ((m,  xK_F4),      spawn "sudo backlight 852")
  , ((m,  xK_t),       sinkAll >> setLayout (XMonad.layoutHook conf))
  , ((m,  xK_a),       sinkAll >> floatAllCurrent)
  , ((m,  xK_f),       sendMessage NextLayout)
  ]
  ++
  [((m .|. mod4Mask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  where
    setmouse = warpToWindow 0.5 0.5
    down = windows W.focusDown >> setmouse
    up = windows W.focusUp >> setmouse
    swapdown = windows W.swapDown >> setmouse
    swapup = windows W.swapUp >> setmouse
    maim = spawn "maim -s ~/maim$(date +%s).png"
    m = mod4Mask
    ms = mod4Mask .|. shiftMask

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings _ = M.fromList
  [ ((mod4Mask, button1), \w -> focus w >> mouseMoveWindow w
                                        >> windows W.shiftMaster)
  , ((mod4Mask, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((mod4Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
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
  spawn "trayer --widthtype request --height 20 --transparent true --tint 0x00000000 --alpha 0"
  host <- runProcessWithInput "hostname" [] ""
  let mag = "mag" `isInfixOf` host
  when mag $ spawn "qbittorrent"

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
