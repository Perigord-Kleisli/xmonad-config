{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import XMonad

import XMonad.Hooks.ManageDocks ()
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.StatusBar.PP ()
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, removeKeysP, removeKeys)

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    $ myConfig

myConfig =
  def
    { modMask = mod4Mask -- Rebind Mod to the Super key
    , layoutHook = myLayout -- Use custom layouts
    , manageHook = myManageHook -- Match on certain windows
    }
    `additionalKeys` [ ((mod4Mask, xK_p), spawn "rofi -show drun")
                     , ((mod4Mask, xK_Return), spawn "kitty")
                     , ((mod4Mask .|. shiftMask, xK_C), kill)
                     ]
    `removeKeys` [(mod4Mask .|. shiftMask, xK_Return)]

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
 where
  threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
  tiled = Tall nmaster delta ratio
  nmaster = 1 -- Default number of windows in the master pane
  ratio = 1 / 2 -- Default proportion of screen occupied by master pane
  delta = 3 / 100 -- Percent of screen to increment by when resizing panes

-- \| Windows should have *some* title, which should not not exceed a
-- sane length.
-- ppWindow :: String -> String
-- ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30
--
-- blue, lowWhite, magenta, red, white, yellow :: String -> String
-- magenta  = xmobarColor "#ff79c6" ""
-- blue     = xmobarColor "#bd93f9" ""
-- white    = xmobarColor "#f8f8f2" ""
-- yellow   = xmobarColor "#f1fa8c" ""
-- red      = xmobarColor "#ff5555" ""
-- lowWhite = xmobarColor "#bbbbbb" ""
