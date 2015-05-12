-- 2015-05-12 https://gist.github.com/doitian/1019203 

{-# OPTIONS_GHC -fcontext-stack=32 #-}
import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import Control.OldException(catchDyn,try)

import Data.Char (toLower)
import Data.List (intercalate, intersperse, isSuffixOf, isPrefixOf)
import qualified Data.Map as M (fromList)

import System.Exit (exitSuccess)
import System.Posix (sleep)

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS (findWorkspace, nextScreen, prevScreen, swapNextScreen, swapPrevScreen, toggleOrDoSkip, WSType(..))
import XMonad.Actions.CycleWindows
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.Promote
import XMonad.Actions.UpdateFocus
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll

import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.Dishes
import XMonad.Layout.DwmStyle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.AutoMaster
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.Magnifier as Mag

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Window

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByIndex)

-- Other usefull package
-- Desktop Gadgets
-- > import XMonad.Layout.Monitor
-- Title Bar
-- > import XMonad.Layout.NoFrillsDecoration

-- Dependencids
-- xdotool, wmctrl

myTerminal      = "urxvt"
myBorderWidth   = 2
myModMask       = mod4Mask
myNumlockMask   = mod2Mask

myNormalBorderColor  = "#4B4B4B"
myFocusedBorderColor = "#60B48A"
myActiveBorderColor  = "#808080"

-- shell prompt theme
mySP = defaultXPConfig
       { font = "xft:WenQuanYi Micro Hei Mono:pixelsize=14"
       , bgColor           = "#2B2B2B"
       , fgColor           = "#DCDCCC"
       , bgHLight          = "#4F4F4F"
       , fgHLight          = "#DCDCCC"
       , borderColor       = "#6F6F6F"
       , promptBorderWidth = 1
       , position          = Top
       , height            = 22
       , defaultText       = []
       }

myAutoSP = mySP { autoComplete       = Just 1000 }
myWaitSP = mySP { autoComplete       = Just 1000000 }

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        c <- withDisplay $ \d -> fmap resClass $ io $ getClassHint d w
        whenJust (W.findTag w ws) (flash name c)
      where flash _ "Pidgin" _ = spawn "true"
            flash _ "emesene" _ = spawn "true"
            flash name c index = spawn $
                                 intercalate " " $
                                 [ "notify-send -i"
                                 , icon
                                 , show $ show name
                                 , show $ "on " ++ index ]
                where icon = case c of
                               "URxvt" -> "gnome-terminal"
                               otherwise -> map toLower c

focusUrgent' :: X ()
focusUrgent' = withUrgents $ (windows . focusOrSwitch)
               where
                 focusOrSwitch (w:_) = W.focusWindow w
                 focusOrSwitch _   = W.greedyView =<< W.tag . head . namedScratchpadFilterOutWorkspace . W.hidden

myDzenPP h = defaultPP
             { ppOutput   = hPutStrLn h
             , ppCurrent  = dzenColor "#0F1A0F" myFocusedBorderColor . pad
             , ppVisible  = dzenColor myFocusedBorderColor "#0F1A0F" . pad . dzenSwitchWs
             , ppUrgent   = dzenColor "#DCA3A3" "" . pad . dzenSwitchWs . dzenStrip
             , ppHidden   = pad . dzenSwitchWs
             , ppLayout   = dzenColor "#dfaf8f" "" . wrap " ^ca(1,xdotool key Super_L+backslash)[" "]^ca() "
             , ppTitle    = dzenColor "#DCDCCC" "" . dzenEscape
             , ppSep      = ""
             , ppWsSep    = "^fg(#0F1A0F)^r(2x16)^fg()"
             , ppSort     = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
             , ppExtras   = [ppScreens]
            }
               where ppScreens = do ws <- gets windowset
                                    let cv = [W.current ws] ++ W.visible ws
                                        tags = map (\w -> (show $ fromIntegral $ W.screen w) ++ "." ++ (W.tag $ W.workspace w)) cv
                                    return $ Just ("\0000" ++ concat (intersperse "\0000" tags))

dzenSwitchWs :: String -> String
dzenSwitchWs s = "^ca(1,switch-workspace.rb " ++ (show s) ++ ")" ++ s ++ "^ca()"

myTheme :: Theme
myTheme = defaultTheme
          { fontName = "xft:WenQuanYi Micro Hei:pixelsize=14"
          , decoHeight = 20
          , decoWidth = 400
          , activeColor = "#1E2320"
          , inactiveColor = "#3F3F3F"
          , urgentColor = "#3F3F3F"
          , activeBorderColor = "#6F6F6F"
          , inactiveBorderColor = "#3F3F3F"
          , urgentBorderColor = "#1E2320"
          , activeTextColor = "#F0DFAF"
          , inactiveTextColor = "#DCDCCC"
          , urgentTextColor = "#CC9393"
          }

pads = [ NS "term" "urxvt -name scratchpad -e sh -l -c 'tmux has -t quake && tmux attach -t quake || tmux new -s quake'" (resource =? "scratchpad" <&&> className =? "URxvt") (customFloating $ W.RationalRect 0.2 0.6 0.6 0.4)
       , NS "stardict" "killall stardict; stardict" (className =? "Stardict") (customFloating $ W.RationalRect 0 0.5 0.4 0.5)
       ]

-- unused char
-- a, x, y, ', m
myKeys =  \conf -> mkKeymap conf $
    [ ("M-S-<Return>", spawn $ XMonad.terminal conf) -- terminal
    , ("M-`", namedScratchpadAction pads "term") -- quake terminal
    , ("M-d", namedScratchpadAction pads "stardict") -- quake dict

    -- prompt
    , ("M-p g", windowPromptGoto myWaitSP) -- window go prompt
    , ("M-p b", windowPromptBring myWaitSP) -- window bring prompt
    , ("M-p d", AL.launchApp mySP { defaultText = "~" } "pcmanfm" ) -- file manager prompt
    , ("M-p o", AL.launchApp mySP "xopen" ) -- open prompt
    , ("M-g", runOrRaise "window-go.sh" (resource =? "WindowGo" <&&> className =? "Gpicker")) -- window go
    , ("M-b", runOrRaise "window-bring.sh" (resource =? "WindowBring" <&&> className =? "Gpicker")) -- window bring
    , ("M-<Return>", raiseMaybe (spawn "gmrun") (className =? "Gmrun")) -- gmrun

    , ("M-S-s", spawn "x-www-browser \"http://www.google.com/search?q=`xclip -o`\"") -- search selection
    , ("M-S-o", spawn "xopen -") -- open current selection

    -- app
    , ("M-o", runOrRaiseNext "firefox" (className =? "Firefox")) -- chrome
    , ("M-i", runOrRaiseNext "emacs-dwim" (className =? "Emacs")) --emacs
    , ("M-u", runOrRaiseNext "urxvt" (className =? "URxvt" <&&> resource /=? "scratchpad")) -- raise next terminal

    , ("M-c t", raiseNextMaybe (spawn "urxvt -name htop -e htop") (resource =? "htop")) -- Top
    , ("M-c f", raiseNextMaybe (spawn "pcmanfm") (resource =? "Pcmanfm")) -- File Manager
    , ("M-c h", spawn "xmonad-key.sh") -- Help
    , ("M-c i", spawn "xp") -- Window Info
    , ("M-c x", spawn "xkill") -- Kill X app
    , ("M-c d", spawn "dropbox stop && dropbox start") -- Restart Dropbox
    , ("M-c S-d", spawn "notify-send -i dropbox `dropbox status`") -- Dropbox Status
    , ("M-c c", runOrRaiseNext "gsimplecal" (className =? "Gsimplecal")) --gsimplecal

    -- client
    , ("M-S-c", kill1) -- kill
    , ("M-C-c", kill) -- kill all
    , ("M-C-S-c", kill) -- kill all
    , ("M-S-<Backspace>", kill1) -- kill
    , ("M-S-<Backspace>", kill1) -- kill
    , ("M-c M-S-c", killAll) -- kill
    , ("M-S-=", windows copyToAll) -- copy to all
    , ("M-=",  killAllOtherCopies) -- kill others
    , ("M-C-S-=",  kill) -- kill all

    -- make sure mod matches keysym
    , ("M-a", rotUnfocusedDown)
    , ("M-S-a", rotUnfocusedUp)
    , ("M-<Tab>", windows W.focusDown) -- focus down
    , ("M-S-<Tab>", windows W.focusUp) -- focus up
    , ("M-C-<Tab>", rotFocusedDown) -- focus down
    , ("M-C-S-<Tab>", rotFocusedUp) -- focus up
    , ("M-<Page_Down>", rotFocusedDown) -- focus down
    , ("M-<Page_Up>", rotFocusedUp) -- focus up
    , ("M-;", windows W.focusMaster) -- focus master
    , ("M-S-;", promote) -- promote to master
    , ("M-z", focusUrgent) -- focus urgent
    , ("M-S-<Page_Down>", windows W.swapDown  ) -- swap down
    , ("M-S-<Page_Up>", windows W.swapUp    ) -- swap up

    , ("M-[", sendMessage Shrink) -- shrink master
    , ("M-]", sendMessage Expand) -- expand master
    , ("M-S-[", sendMessage MirrorShrink) -- shrink window in slave pane
    , ("M-S-]", sendMessage MirrorExpand) -- expand window in slave pane

    , ("M-h", sendMessage $ Go L) -- focus left
    , ("M-j", sendMessage $ Go D) -- focus down
    , ("M-k", sendMessage $ Go U) -- focus up
    , ("M-l", sendMessage $ Go R) -- focus right
    , ("M-S-h", sendMessage $ Swap L) -- swap left
    , ("M-S-j", sendMessage $ Swap D) -- swap down
    , ("M-S-k", sendMessage $ Swap U) -- swap up
    , ("M-S-l", sendMessage $ Swap R) -- swap right
    , ("M-C-h", sendMessage $ Move L) -- move left
    , ("M-C-j", sendMessage $ Move D) -- move down
    , ("M-C-k", sendMessage $ Move U) -- move up
    , ("M-C-l", sendMessage $ Move R) -- move right

    -- float
    , ("M-<L>", withFocused (keysMoveWindow (-20,0))) -- move float left
    , ("M-<R>", withFocused (keysMoveWindow (20,0))) -- move float right
    , ("M-<U>", withFocused (keysMoveWindow (0,-20))) -- move float up
    , ("M-<D>", withFocused (keysMoveWindow (0,20))) -- move float down
    , ("M-S-<L>", withFocused (keysResizeWindow (-20,0) (0,0))) --shrink float at right
    , ("M-S-<R>", withFocused (keysResizeWindow (20,0) (0,0))) --expand float at right
    , ("M-S-<D>", withFocused (keysResizeWindow (0,20) (0,0))) --expand float at bottom
    , ("M-S-<U>", withFocused (keysResizeWindow (0,-20) (0,0))) --shrink float at bottom
    , ("M-C-<L>", withFocused (keysResizeWindow (20,0) (1,0))) --expand float at left
    , ("M-C-<R>", withFocused (keysResizeWindow (-20,0) (1,0))) --shrink float at left
    , ("M-C-<U>", withFocused (keysResizeWindow (0,20) (0,1))) --expand float at top
    , ("M-C-<D>", withFocused (keysResizeWindow (0,-20) (0,1))) --shrink float at top

    -- layout
    , ("M-\\", sendMessage NextLayout) -- toggle layouts
    , ("M-S-\\", myLayoutPrompt) -- layout prompt
    , ("M-C-\\", setLayout $ XMonad.layoutHook conf) -- reset layout
    , ("M-f", sendMessage (Toggle "Full")) -- toggle Full
    , ("M-<Escape>", sendMessage ToggleStruts) -- toggle panel
    , ("M-t", withFocused $ windows . W.sink) -- sink focused window
    , ("M-S-t", sinkAll) -- sink all windows
    , ("M-C-[", sendMessage (IncMasterN 1)) -- increase master windows number
    , ("M-C-]", sendMessage (IncMasterN (-1))) --decrease master windows number

    -- system
    , ("M-C-S-q", io (exitHook >> exitSuccess)) -- exit
    , ("M-S-q",  broadcastMessage ReleaseResources >> io exitHook >> restart "xmonad" True) -- restart
    , ("M-c M-C-S-q", io $ runProcessWithInput "emacs-dwim" ["-q"] "" >> spawn "gksu /sbin/poweroff") -- poweroff
    , ("M-c M-C-S-r", io $ runProcessWithInput "emacs-dwim" ["-q"] "" >> spawn "gksu /sbin/reboot") -- reboot
    , ("M-q", refresh) -- refresh

    -- cycle through workspaces
    , ("M-n", withWorkspace' (windows . W.greedyView)) -- workspace prompt gready
    , ("M-m", withWorkspace' (windows . W.view)) -- workspace prompt
    , ("M-S-n", withWorkspace' (windows . W.shift)) -- workspace shift prompt
    , ("M-C-n", withWorkspace' (windows . copy)) -- workspace copy prompt
    , ("M-C-S-n", renameWorkspace mySP) -- rename workspace
    , ("M-C-S-<Backspace>", removeWorkspace) -- delete empty workspace
    , ("M-/", toggleWSNoSP) -- toggle recently visited workspaces
    ]
    ++
    -- "M-[1..9,0,-]" -- Switch to workspace N
    -- "M-S-[1..9,0,-]" -- Move client to workspace N
    -- "M-C-[1..9,0,-]" -- Copy client to workspace N
    [("M-" ++ m ++ [k], windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'] ++ ['0', '-'])
        , (f, m) <- [ (W.greedyView, "")
                    , (W.shift, "S-")
                    , (copy, "C-")
                    ]
    ]
    ++
    -- "M-C-S-[1..9,0,-]" -- Move client to workspace N and follow
    [("M-C-S-" ++ [k], (windows $ W.shift i) >> (windows $ W.greedyView i))
        | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'] ++ ['0', '-'])
    ]
    ++
    -- "M-{w,e,r}" -- Switch to physical/Xinerama screens 1, 2, or 3
    -- "M-S-{w,e,r}" -- Move client to screen 1, 2, or 3
    --
    [("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "S-")]]
    ++
    [ ("M-'", prevScreen) -- Prev Screen
    , ("M-S-'", swapNextScreen) -- Swap next screen
    ]
    ++
    -- HiddenNonEmptyWS
    [ ("M-.", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1) -- go to next workspace
    , ("M-,", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1) -- go to prev workspace
    , ("M-S-.", windows . W.shift =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1) -- shift to next workspace
    , ("M-S-,", windows . W.shift =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1) -- shift to prev workspace
    -- move window to and focus HiddenNonEmpty wss except scratchpad
    , ("M-C-S-.", shiftAndView' Next) -- shift to next workspace and follow
    , ("M-C-S-,", shiftAndView' Prev) -- shift to prev workspace and follow
    ]
    where
      getSortByIndexNoSP =
          fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
      shiftAndView' dir = findWorkspace getSortByIndexNoSP dir AnyWS 1
                          >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
      toggleWSNoSP = windows $ W.greedyView =<< W.tag . head . namedScratchpadFilterOutWorkspace . W.hidden
      role = stringProperty "WM_WINDOW_ROLE"

dmenuXinerama :: [String] -> X String
dmenuXinerama opts = do
    curscreen <- (fromIntegral . W.screen . W.current) `fmap` gets windowset :: X Int
    io $ runProcessWithInput "dmenu" ["-m", show curscreen] (unlines opts)

withWorkspace' :: (String -> X ()) -> X ()
withWorkspace' job = do sort <- fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
                        ws <- gets (map W.tag . sort . W.workspaces . windowset)
                        w <- dmenuXinerama ws
                        let w' = case reverse w of
                              '\n':xs -> reverse xs
                              otherwise -> w
                          in case w' of (_:_)
                                         | w' `elem` ws -> job w'
                                         | otherwise -> addHiddenWorkspace w' >> job w'

myLayoutPrompt :: X ()
myLayoutPrompt = do l <- dmenuXinerama layouts
                    case reverse l of
                      '\n':xs -> let l' = reverse xs
                                 in sendMessage $ JumpToLayout $ drop 2 l'
                      where
                        layouts = ["1.tall", "2.wide", "3.2col", "4.2row", "5.tab", "6.grid", "7.big", "8.dish"]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    -- forward,backward button8 and button9
    , ((0, 8), (\w -> windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1)) -- go to next workspace
    , ((0, 9), (\w -> windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1)) -- go to prev workspace
    ]
    where
      getSortByIndexNoSP =
          fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

------------------------------------------------------------------------
-- Layouts:
myLayout = avoidStruts
           $ configurableNavigation (navigateColor myActiveBorderColor)
           $ toggleLayouts (Full)
           $ smartBorders
           $ onWorkspace "7.gimp" gimp'
           $ onWorkspace "9.im" im'
           $ onWorkspace "-" magG'
           $ layouts
  where
    layouts  = tall' ||| wide' ||| col2' ||| row2' ||| tabs' ||| magG' ||| big' ||| dish'
    tall'    = named "tall"  $ layoutHints $ deco tall
    wide'    = named "wide"  $ layoutHints $ deco wide
    col2'    = named "2col"  $ layoutHints $ deco $ TwoPane delta ratio
    row2'    = named "2row"  $ layoutHints $ deco $ Mirror $ TwoPane delta ratio
    tabs'    = named "tab"   $ layoutHints $ tabs
    magG'    = named "grid"  $ layoutHints $ deco magG
    masterG' = named "grid"  $ layoutHints $ deco masterG
    big'     = named "big"   $ layoutHints $ deco $ OneBig (3/4) (3/4)
    dish'    = named "dish"  $ layoutHints $ deco dish
    -- per work space layouts, single quote indicates modifiers
    gimp'    = named "gimp"  $ layoutHints $ gimp
    im'      = named "im"    $ layoutHints $ im
    -- basic layouts
    dish     = Dishes nmaster (1/6)
    tall     = ResizableTall nmaster delta ratio []
    wide     = Mirror tall
    tabs     = tabbed shrinkText myTheme
    masterG  = Mirror $ autoMaster nmaster (fromRational delta) $ Mirror magG
    magG     = Mag.magnifiercz 1.15 Grid
    deco     = dwmStyle shrinkText myTheme
    gimp     = withIM (0.2) (Role "gimp-toolbox")
               $ reflectHoriz
               $ withIM (0.18) (Role "gimp-dock") tabs
    im       = withIM (1/7) imRoster $ reflectHoriz $ withIM (1/5) imRightRoster Grid
    -- parameters
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100
    imRoster = (ClassName "emesene" `And` Role "main") 
               `Or` (ClassName "Pidgin" `And` Role "buddy_list")
               `Or` (ClassName "Empathy" `And` Role "contact_list")
    imRightRoster = (ClassName "Skype" `And` Role "MainWindow")

myWorkspaces    = ["1.sys","2.www","3.emacs","4.doc","5","6","7.gimp","8.sns","9.im","0","-"]

myFloatManageHook = composeOne . concat $
    [ [ (isFullscreen -?> doFullFloat')
      , (isDialog -?> doCenterFloat')
      , (className =? "Emacs" <&&> resource =? "Ediff" -?> doFloat')
      , (className =? "Pidgin" <&&> resource =? "multifield" -?> doFloat')
      , (className =? "Gsimplecal" -?> doRectFloat' (W.RationalRect 0.75 0.02 0.25 0.23))
      ]
    , [ className =? x -?> doFloat' | x <- cFloat ]
    , [ className =? x -?> doCenterFloat' | x <- cCenter ]
    , [ role =? x -?> doFloat' | x <- rFloat ]
    , [ (className =? "Minefield" <||> className =? "Firefox") <&&> resource =? x -?> doCenterFloat'
        | x <- ffCenter
      ]
    , [ className =? x -?> doMaster | x <- masters   ]
    , [ className =? "Pidgin" <&&> role =? "buddy_list" -?> doMaster ]
    -- , [ return True -?> doF W.swapDown ] -- prevent changing master
    ]
    where
      unFloat = ask >>= doF . W.sink
      cFloat  = [ "Zenity", "Airappinstaller",
                  "Gcolor2", "Gpicker", "Stardict", "Update-manager", "Shutter" ]
      rFloat  = ["gimp-toolbox-color-dialog", "gimp-brightness-contrast-tool", "gimp-color-selector"]
      ffCenter = [ "Manager", "Extension", "Download", "Dialog", "Browser", "Toplevel" ]
      cCenter = [ "Gmrun" ]
      masters = [ "Emacs" ]
      doMaster = doF W.shiftMaster
      doFloat' = doFloat <+> doMaster
      doCenterFloat' = doCenterFloat <+> doMaster
      doFullFloat' = doFullFloat <+> doMaster
      doRectFloat' r = doRectFloat r <+> doMaster
      role = stringProperty "WM_WINDOW_ROLE"

myShiftManageHook = composeOne . concat $
    [ [ transience ]
    , [ className =? c -?> doShift t
        | (c, t) <- [ ("Pidgin", "9.im")
                    , ("Skype", "9.im")
                    , ("Gimp-2.6", "7.gimp")
                    , ("Gimp", "7.gimp")
                    , ("Emacs", "3.emacs")
                    ]
      ]
    , [ resource =? r -?> doShift t
        | (r, t) <- [ ("DTA", "0") ]
      ]
    ]

myManageHook = namedScratchpadManageHook pads
               <+> myShiftManageHook
               <+> myFloatManageHook
               <+> manageDocks

myHandleEventHook = focusOnMouseMove

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = adjustEventInput
                >> setWMName "LG3D"

exitHook :: IO ()
exitHook = do
  return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

main = do
  dzen <- spawnPipe "xmonad.panel"
  xmonad $ ewmh $ withUrgencyHook LibNotifyUrgencyHook defaultConfig
             { terminal           = myTerminal
             , focusFollowsMouse  = myFocusFollowsMouse
             , borderWidth        = myBorderWidth
             , modMask            = myModMask
             , numlockMask        = myNumlockMask
             , workspaces         = myWorkspaces
             , normalBorderColor  = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor

             -- key bindings
             , keys               = myKeys
             , mouseBindings      = myMouseBindings

             -- hooks, layouts
             , layoutHook         = myLayout
             , manageHook         = myManageHook
             , handleEventHook    = myHandleEventHook
             , logHook            = (dynamicLogWithPP $ myDzenPP dzen)
                                    >> setWMName "LG3D"
             , startupHook        = myStartupHook
             }
             `additionalKeys`
             [ ((0, 0x1008ff18), AL.launchApp mySP { defaultText = "~" } "pcmanfm")
             , ((0, 0x1008ff1b), spawn "xopen -")
             , ((0, 0x1008ff19), namedScratchpadAction pads "mutt")
             , ((0, 0x1008ff30), spawn "goodsong add")
             , ((mod4Mask, 0x1008ff30), spawn "goodsong play")
             , ((0, 0x1008ff1d), runOrRaise "gcalctool" (className =? "Gcalctool"))
             , ((0, 0x1008ff26), spawn "mpc-osd prev")
             , ((0, 0x1008ff27), spawn "mpc-osd next")
             , ((0, 0x1008ff14), spawn "mpc-osd toggle")
             , ((mod4Mask, 0x1008ff26), spawn "mpc-osd seek -5")
             , ((mod4Mask, 0x1008ff27), spawn "mpc-osd seek +5")
             , ((0, 0x1008ff13), spawn "volume-osd 2+")
             , ((0, 0x1008ff11), spawn "volume-osd 2-")
             , ((0, 0x1008ff12), spawn "volume-osd toggle")
             , ((mod4Mask, 0x1008ff13), spawn "mpc-osd volume +5")
             , ((mod4Mask, 0x1008ff11), spawn "mpc-osd volume -5")
             , ((0, 0xff14), spawn "xscreensaver-command -lock || gnome-screensaver-command --lock")
             , ((mod4Mask, 0xff61), spawn "shutter --full")
             , ((mod4Mask .|. controlMask, 0xff61), spawn "shutter --selection")
             ]
