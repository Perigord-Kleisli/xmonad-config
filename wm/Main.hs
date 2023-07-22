{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import System.Environment.XDG.BaseDir (getUserCacheFile, getUserConfigDir)
import XMonad
import XMonad.Actions.WindowMenu (windowMenu)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar (defToggleStrutsKey, statusBarGeneric, withEasySB)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Util.EZConfig (additionalKeysP)

import PyF

main :: IO ()
main = do
  taffyBarPath <- getUserCacheFile "xmonad" "taffybar"
  config_home <- getUserConfigDir "xmonad"
  rofi_scripts <- (<> "/rofi-scripts") <$> getUserConfigDir "xmonad"

  let myConfig =
        def
          { modMask = mod4Mask -- Rebind Mod to the Super key
          , layoutHook =
              smartSpacingWithEdge 4
                . lessBorders OtherIndicated
                $ avoidStruts myLayout -- Use custom layouts
          , manageHook = myManageHook
          , terminal = "kitty"
          , focusedBorderColor = "#5277C3"
          , normalBorderColor = "#636363"
          }
          `additionalKeysP` [ ("M-p", spawn [fmt|rofi -show drun -theme {rofi_scripts}/launcher-style.rasi|])
                            , ("M-S-p", spawn [fmt|rofi -show run -theme {rofi_scripts}/launcher-style.rasi|])
                            , ("M-C-q", spawn [fmt|CONFIG_HOME={config_home} {rofi_scripts}/powermenu/powermenu.bash|])
                            , ("M-S-s", spawn "flameshot gui")
                            , ("M-S-x", kill)
                            , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-")
                            , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+")
                            , ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
                            ]
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    . withEasySB (statusBarGeneric taffyBarPath mempty) defToggleStrutsKey
    . pagerHints
    $ myConfig

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog --> doFloat
    ]

myLayout =
  tiled
    ||| Mirror tiled
    ||| noBorders Full
    ||| threeCol
 where
  threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
  tiled = Tall nmaster delta ratio
  nmaster = 1 -- Default number of windows in the master pane
  ratio = 1 / 2 -- Default proportion of screen occupied by master pane
  delta = 3 / 100 -- Percent of screen to increment by when resizing panes
