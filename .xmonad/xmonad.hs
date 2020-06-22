--   _____ ______ _____
--  / ____|  ____|  __ \  || Programming enthausiast, privacy advocate, free software user.
-- | (___ | |__  | |__) | ||
--  \___ \|  __| |  _  /  || github.com/SFR-git
--  ____) | |    | | \ \  || ko-fi.com/supportsfr
-- |_____/|_|    |_|  \_\ || /u/sfrvtma

-- Imports
    -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)

    -- Data
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.EwmhDesktops 
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName

    -- Layout
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
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Util
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-- Variables
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor = "#1a1a1a"

myFocusColor :: String
myFocusColor = "#fafafa"

altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Autostart
myStartupHook :: X ()
myStartupHook = do
          spawnOnce "feh --randomize --bg-fill /usr/share/wallpapers/* &" 
          spawnOnce "picom &"
          spawnOnce "udiskie -Nt &"
          spawnOnce "nm-applet &"
          spawnOnce "volumeicon &"
          spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x292d3e --height 18 &"
          spawnOnce "setxkbmap gb,il &"
          spawnOnce "sleep 1; xmodmap ~/.Xmodmap &"
          setWMName "LG3D"

-- Keybinds
myKeys :: [(String, X ())]
myKeys =
    -- xmonad
        [ ("M-C-r", spawn "xmonad --recompile")                        -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")                          -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                                    -- Quits xmonad
    
    -- Window management
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

    -- Miscellaneous
        , ("M-<Return>", spawn (myTerminal))                           -- Open Terminal
        , ("<F13>", spawn "~/.config/rofi/launcher/launcher.sh")       -- Application Launcher
        , ("<F15>", spawn "xkb-switch -n && xmodmap ~/.Xmodmap")       -- Cycle keyboard layouts

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
       , ("M-S-<Space>", sendMessage ToggleStruts)         -- Toggles struts
       , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)      -- Toggles noborder
       , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in master pane
       , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in master pane
       , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows
       , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows
       ]
                     
-- Workspaces
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]
        
myWorkspaces :: [String]   
myWorkspaces = clickable
               $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  where                                                                      
        clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,                                        
                      let n = i ] 

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ title =? "Oracle VM VirtualBox Manager"     --> doFloat
     , className =? "Oracle VM VirtualBox Manager" --> doShift  ( myWorkspaces !! 6)
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ] 

-- Layouts
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
          
    -- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               -- I've commented out the layouts I don't use.
               myDefaultLayout =     smartBorders tall
                                 ||| floats

-- Main
main :: IO ()
main = do
    xmproc <- spawnPipe "polybar bar1" -- Launch Polybar
    xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        , handleEventHook    = serverModeEventHookCmd 
                               <+> serverModeEventHook 
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook 
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor                         
        } `additionalKeysP` myKeys 

