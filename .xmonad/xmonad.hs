--   _____ ______ _____
--  / ____|  ____|  __ \  || Programming enthausiast, privacy advocate, free software user.
-- | (___ | |__  | |__) | ||
--  \___ \|  __| |  _  /  || github.com/SFR-git
--  ____) | |    | | \ \  || ko-fi.com/supportsfr
-- |_____/|_|    |_|  \_\ || /u/sfrvtma

import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import qualified Data.Map as M
import XMonad.Hooks.EwmhDesktops 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

-- Layouts (NOT DONE)
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "Master & Stacked"]
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "Monocle"]
           $ mySpacing 4
           $ limitWindows 20 Full
grid     = renamed [Replace "Grid"]
           $ limitWindows 12
           $ mySpacing 4
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
wide    = renamed [Replace "Vertical M&S"]
           $ limitWindows 12
           $ mySpacing 4
           $ Mirror
           $ ResizableTall 1 (3/100) (1/2) []
floats   = renamed [Replace "Floating"]
           $ limitWindows 20 simplestFloat
          
-- Main
main :: IO ()
main = do
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad $ ewmh $ docks $ def
        { modMask            = mod4Mask
        , terminal           = "alacritty"

    -- Autostart
        , startupHook = do
            spawnOnce "feh --randomize --bg-fill /usr/share/wallpapers/* &" 
            spawnOnce "picom &"
            spawnOnce "udiskie -Nt &"
            spawnOnce "polybar bar1 &"
            spawnOnce "setxkbmap gb,il &"
            spawnOnce "sleep 1; xmodmap ~/.Xmodmap &"
            setWMName "LG3D"

    -- Layouts (NOT DONE)
        , layoutHook = 
            avoidStruts $ mouseResize $ smartBorders $ windowArrange
                tall
            ||| monocle
            ||| wide
            ||| grid
            ||| floats

    -- Workspaces
        , workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

    -- Border
        , normalBorderColor  = "#1a1a1a"
        , focusedBorderColor = "#fafafa"

    -- Polybar
        , logHook = dynamicLogWithPP (def
            { ppOutput = dbusOutput dbus
            , ppCurrent = wrap ("%{F" ++ "#fafafa" ++ "} ") " %{F-}"
            , ppVisible = wrap ("%{F" ++ "#fafafa" ++ "} ") " %{F-}"
            , ppHidden = wrap ("%{F" ++ "#545454" ++ "} ") " %{F-}"
            , ppSep = " | "
            , ppWsSep = ""
            })

    -- Keybinds
        } `additionalKeysP` 
            [ ("M-C-r", spawn "xmonad --recompile")                        -- Recompiles xmonad
            , ("M-S-r", spawn "xmonad --restart")                          -- Restarts xmonad
            , ("M-S-q", io exitSuccess)                                    -- Quits xmonad
            , ("M-<Return>", spawn "alacritty")                            -- Open Alacritty
            , ("M-S-c", kill1)                                             -- Kill the currently focused client
            , ("M-S-a", killAll)                                           -- Kill all windows on current workspace
            , ("M-m", windows W.focusMaster)                               -- Move focus to the master window
            , ("M-j", windows W.focusDown)                                 -- Move focus to the next window
            , ("M-k", windows W.focusUp)                                   -- Move focus to the previous window
            , ("M-S-j", windows W.swapDown)                                -- Swap focused window with next window
            , ("M-S-k", windows W.swapUp)                                  -- Swap focused window with previous window
            , ("M-f", sendMessage (T.Toggle "floats"))                     -- Make window float
            , ("M-`", withFocused $ windows . W.sink)                      -- Push floating window back to tile
            , ("M-S-`", sinkAll)                                           -- Push all floating windows to tile
            , ("M-h", sendMessage Shrink)                                  -- Shrink horizontal window width
            , ("M-l", sendMessage Expand)                                  -- Expand horizontal window width
            , ("M-C-j", sendMessage MirrorShrink)                          -- Shrink vertical window width
            , ("M-C-k", sendMessage MirrorExpand)                          -- Expand vertical window width
            , ("M-<Tab>", sendMessage NextLayout)                          -- Switch to next layout
            , ("<F13>", spawn "~/.config/rofi/launcher/launcher.sh")       -- Application Launcher
            , ("<F14>", spawn "scrot ~/Pictures/Screenshots/%b%d-%H%M%S.png") -- Take a fullscreen screenshot
            , ("S-<F14>", spawn "sleep 0.2; scrot -s ~/Pictures/Screenshots/%b%d-%H%M%S.png") -- Take screenshot of area
            , ("<F15>", spawn "xkb-switch -n && xmodmap ~/.Xmodmap")       -- Cycle keyboard layouts
            ] 

-- (NOT DONE)
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
