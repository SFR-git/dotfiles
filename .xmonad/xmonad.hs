--   _____ ______ _____
--  / ____|  ____|  __ \  || Programming enthausiast, privacy advocate, free software user.
-- | (___ | |__  | |__) | ||
--  \___ \|  __| |  _  /  || github.com/SFR-git
--  ____) | |    | | \ \  || ko-fi.com/supportsfr
-- |_____/|_|    |_|  \_\ || /u/sfrvtma

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified DBus as D
import qualified DBus.Client as D
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce

main :: IO ()
main = do
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad $ ewmh $ docks $ def
        { modMask            = mod4Mask                                     -- Set Super as mod key
        , terminal           = "alacritty"                                  -- Set terminal emulator to Alacritty

    -- Autostart
        , startupHook = do
            spawnOnce "xsetroot -cursor_name left_ptr &"                    -- Replace X cursor
            spawnOnce "feh --randomize --bg-fill /usr/share/wallpapers/* &" -- Set random wallpaper
            spawnOnce "picom &"                                             -- Compositing
            spawnOnce "udiskie -Nt &"                                       -- Automount disks
            spawnOnce "polybar bar1 &"                                      -- Polybar
            spawnOnce "setxkbmap gb,il &"                                   -- Layout switcher
            spawnOnce "sleep 1; xmodmap ~/.Xmodmap &"                       -- Key remaps & inverse scrolling
            spawnOnce "xset s off -dpms &"                                  -- Disable screensaver
            setWMName "LG3D"                                                -- Java compatibility iirc

    -- Layouts
        , layoutHook = do
            avoidStruts $ mouseResize $ smartBorders $ windowArrange $
                    (( renamed [Replace "Master & Stacked"]                 -- Master & Stacked
                         $ limitWindows 12
                         $ spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
                         $ ResizableTall 1 (3/100) (1/2) []
              ) ||| (( renamed [Replace "Monocle"]                          -- Fullscreen
                         $ spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
                         $ limitWindows 20 Full
              ) ||| (( renamed [Replace "Grid"]                             -- Grid
                         $ limitWindows 12
                         $ spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
                         $ mkToggle (single MIRROR)
                         $ Grid (16/10)
              ) ||| (( renamed [Replace "Vertical M&S"]                     -- Master & Stacked with Master on top
                         $ limitWindows 12
                         $ spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
                         $ Mirror
                         $ ResizableTall 1 (3/100) (1/2) []
              ) ||| (( renamed [Replace "Floating"]                         -- Floating mode
                         $ limitWindows 20 simplestFloat
              ))))))

    -- Workspaces
        , workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"] 
        -- Might add window rules at some point

    -- Border
        , normalBorderColor  = "#1a1a1a" -- Trying to make this transparent
        , focusedBorderColor = "#fafafa"

    -- Polybar
        , logHook = dynamicLogWithPP (def
            { ppOutput = \str -> 
                D.emit dbus ((D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
                        D.signalBody = [D.toVariant $ UTF8.decodeString str]
                    })
            , ppCurrent = wrap ("%{F" ++ "#fafafa" ++ "} ") " %{F-}"        -- Current workspace number colour
            , ppVisible = wrap ("%{F" ++ "#fafafa" ++ "} ") " %{F-}"        -- Workspaces in other screens
            , ppHidden = wrap ("%{F" ++ "#545454" ++ "} ") " %{F-}"         -- Hidden workspaces with open programs
            , ppSep = " | "                                                 -- Module separator
            , ppWsSep = ""                                                  -- Workspace number separator
            })

    -- Keybinds
        } `additionalKeysP` 
            [ ("M-C-r", spawn "xmonad --recompile")                         -- Recompiles xmonad
            , ("M-S-r", spawn "xmonad --restart")                           -- Restarts xmonad
            , ("M-S-q", io exitSuccess)                                     -- Quits xmonad
            , ("M-<Return>", spawn "alacritty")                             -- Open Alacritty
            , ("M-S-c", kill1)                                              -- Kill the currently focused client
            , ("M-S-a", killAll)                                            -- Kill all windows on current workspace
            , ("M-m", windows W.focusMaster)                                -- Move focus to the master window
            , ("M-j", windows W.focusDown)                                  -- Move focus to the next window
            , ("M-k", windows W.focusUp)                                    -- Move focus to the previous window
            , ("M-S-j", windows W.swapDown)                                 -- Swap focused window with next window
            , ("M-S-k", windows W.swapUp)                                   -- Swap focused window with previous window
            , ("M-f", sendMessage (T.Toggle "floats"))                      -- Make window float
            , ("M-`", withFocused $ windows . W.sink)                       -- Push floating window back to tile
            , ("M-S-`", sinkAll)                                            -- Push all floating windows to tile
            , ("M-h", sendMessage Shrink)                                   -- Shrink horizontal window width
            , ("M-l", sendMessage Expand)                                   -- Expand horizontal window width
            , ("M-C-j", sendMessage MirrorShrink)                           -- Shrink vertical window width
            , ("M-C-k", sendMessage MirrorExpand)                           -- Expand vertical window width
            , ("M-<Tab>", sendMessage NextLayout)                           -- Switch to next layout
            , ("<F13>", spawn "~/.config/rofi/launcher/launcher.sh")        -- Application Launcher
            , ("<F14>", spawn "scrot ~/Pictures/Screenshots/%b%d-%H%M%S.png") -- Take a fullscreen screenshot
            , ("S-<F14>", spawn "sleep 0.2; scrot -s ~/Pictures/Screenshots/%b%d-%H%M%S.png") -- Take screenshot of area
            , ("<F15>", spawn "xkb-switch -n && xmodmap ~/.Xmodmap")        -- Cycle keyboard layouts
            ] 
