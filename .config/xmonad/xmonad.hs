--   _____ ______ _____
--  / ____|  ____|  __ \  || Programming enthausiast, privacy advocate, free software user.
-- | (___ | |__  | |__) | ||
--  \___ \|  __| |  _  /  || github.com/SFR-git
--  ____) | |    | | \ \  || ko-fi.com/supportsfr
-- |_____/|_|    |_|  \_\ || /u/sfrvtma

import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.LimitWindows (limitWindows)
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout(Toggle))
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(MIRROR))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce

main :: IO ()
main = do 
    dbus <- D.connectSession                                                -- DBus configuration
    _ <- D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad $ ewmh $ docks $ def
        { modMask            = mod4Mask                                     -- Set Super as mod key
        , terminal           = "alacritty"                                  -- Set terminal emulator to Alacritty

    -- Autostart
        , startupHook = do
            spawnOnce "picom &"                                             -- Compositing
            spawnOnce "polybar bar1 &"                                      -- Polybar
            spawnOnce "feh --randomize --bg-fill /usr/share/wallpapers/* &" -- Set random wallpaper
            spawnOnce "xsetroot -cursor_name left_ptr &"                    -- Replace X cursor
            spawnOnce "setxkbmap us,il -option grp:sclk_toggle &"           -- Layout switcher
            spawnOnce "xset s off -dpms &"                                  -- Disable screensaver
            spawnOnce "udiskie -Nt &"                                       -- Automount disks
            -- spawnOnce "light-locker --lock-on-lid --lock-on-suspend &"   -- Lock on lid close
            setWMName "LG3D"                                                -- Java compatibility iirc

    -- Layouts
        , layoutHook = do
            avoidStruts $ mouseResize $ smartBorders $ windowArrange $
                    (( renamed [Replace "Master & Stacked"]                 -- Master & Stacked
                         $ limitWindows 12
                         $ spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
                         $ ResizableTall 1 (3/100) (4/7) []
              ) ||| (( renamed [Replace "Monocle"]                          -- Fullscreen
                         $ spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
                         $ limitWindows 20 Full
              ) ||| (( renamed [Replace "Floating"]                         -- Floating mode
                         $ limitWindows 20 simplestFloat
              ))))

    -- Workspaces
        , workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"] 
        -- Might add window rules at some point

    -- Fullscreen
        , handleEventHook = docksEventHook <+> handleEventHook def <+> fullscreenEventHook

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
            , ppHidden = wrap ("%{F" ++ "#545454" ++ "} ") " %{F-}"         -- Hidden workspaces with open programs
            , ppSep = " | "                                                 -- Module separator
            , ppWsSep = ""                                                  -- Workspace number separator
            })

    -- Keybinds
        } `additionalKeysP` 
            [ ("M-S-s", spawn "shutdown now")                               -- Shuts the PC down
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
            , ("M-p", spawn "~/.config/rofi/launcher.sh")                   -- Application Launcher
            , ("<Print>", spawn "scrot ~/Pictures/Screenshots/%y-%m-%d-%H%M%S.png") -- Take a fullscreen screenshot
            , ("S-<Print>", spawn "sleep 0.2; scrot -s ~/Pictures/Screenshots/%y-%m-%d-%H%M%S.png") -- Take screenshot of area
            , ("<XF86AudioMute>", spawn "amixer sset Master toggle")        -- Mute audio
            , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 5%- unmute") -- Lower volume
            , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 5%+ unmute") -- Raise volume
            , ("<XF86AudioMicMute>", spawn "amixer sset Capture toggle")    -- Mute mic
            , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")        -- Decrease brightness
            , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")          -- Increase brightness
            ] 
