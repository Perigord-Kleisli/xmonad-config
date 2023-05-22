{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import qualified Data.Map as M
import System.Environment.XDG.BaseDir
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, docks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar (defToggleStrutsKey, statusBarGeneric, withEasySB)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.Decoration
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Util.EZConfig (additionalKeysP)

withStrutsKey :: LayoutClass l Window => (XConfig Layout -> (KeyMask, KeySym)) -> XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
withStrutsKey key conf =
  conf
    { layoutHook = avoidStruts (layoutHook conf)
    , keys = (<>) <$> keys' <*> keys conf
    }
 where
  k' conf' = case key conf' of
    (0, 0) ->
      -- This usually means the user passed 'def' for the keybinding
      -- function, and is otherwise meaningless to harmful depending on
      -- whether 383ffb7 has been applied to xmonad or not. So do what
      -- they probably intend.
      --
      -- A user who wants no keybinding function should probably use
      -- 'withSB' instead, especially since NoSymbol didn't do anything
      -- sane before 383ffb7. ++bsa
      defToggleStrutsKey conf'
    key' -> key'
  keys' = (`M.singleton` sendMessage ToggleStruts) . k'

main :: IO ()
main = do
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    . withStrutsKey defToggleStrutsKey
    . pagerHints
    $ myConfig

myConfig =
  def
    { modMask = mod4Mask -- Rebind Mod to the Super key
    , layoutHook = lessBorders OtherIndicated $ avoidStruts myLayout -- Use custom layouts
    , manageHook = myManageHook
    , terminal = "kitty"
    , focusedBorderColor = "#ff5555"
    , normalBorderColor = "#bbbbbb"
    }
    `additionalKeysP` [ ("M-p", spawn "rofi -show drun")
                      , ("M-S-s", spawn "flameshot gui")
                      , ("M-S-x", kill)
                      , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-")
                      , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+")
                      , ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
                      ]

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| noBorders Full ||| threeCol
 where
  threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
  tiled = Tall nmaster delta ratio
  nmaster = 1 -- Default number of windows in the master pane
  ratio = 1 / 2 -- Default proportion of screen occupied by master pane
  delta = 3 / 100 -- Percent of screen to increment by when resizing panes
