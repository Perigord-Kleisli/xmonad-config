{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import System.Environment.XDG.BaseDir (getUserCacheFile, getUserConfigDir)
import XMonad
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

import Control.Monad
import PyF
import XMonad.Actions.Volume
import XMonad.Util.Brightness qualified as Brightness

main :: IO ()
main = do
  taffyBarPath <- getUserCacheFile "xmonad" "taffybar"
  config_home <- getUserConfigDir "xmonad"
  let rofi_scripts = config_home <> "/rofi-scripts"
      myConfig =
        def
          { modMask = mod4Mask -- Rebind Mod to the Super key
          , layoutHook =
              spacingWithEdge 4
                . lessBorders OtherIndicated
                $ avoidStruts myLayout -- Use custom layouts
          , manageHook = myManageHook
          , terminal = "kitty"
          , focusedBorderColor = "#5277C3"
          , normalBorderColor = "#636363"
          , startupHook = do
              spawn [fmt|feh --no-fehbg --bg-fill {config_home}/data/background.png|]
          }
          `additionalKeysP` [ ("M-p", spawn [fmt|rofi -show drun -theme {rofi_scripts}/launcher-style.rasi|])
                            , ("M-S-p", spawn [fmt|rofi -show run -theme {rofi_scripts}/launcher-style.rasi|])
                            , ("M-S-q", spawn [fmt|CONFIG_HOME={config_home} {rofi_scripts}/powermenu/powermenu.bash|])
                            , ("M-S-s", spawn "flameshot gui")
                            , ("<XF86MonBrightnessUp>", Brightness.increase)
                            , ("<XF86MonBrightnessDown>", Brightness.decrease)
                            ,
                              ( "<XF86AudioLowerVolume>"
                              , void $ lowerVolume 2
                              )
                            ,
                              ( "<XF86AudioRaiseVolume>"
                              , void $ raiseVolume 2
                              )
                            ,
                              ( "<XF86AudioMute>"
                              , do
                                  void toggleMute
                              )
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
