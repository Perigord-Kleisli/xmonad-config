{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Text qualified
import GI.Gtk qualified as Gtk
import StatusNotifier.Tray
import System.Environment.XDG.BaseDir
import System.Log.Logger
import System.Taffybar
import System.Taffybar.DBus
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Util
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph

main :: IO ()
main = do
  enableLogger "Graphics.UI.GIGtkStrut" DEBUG
  cssFiles <- getUserConfigFile "taffybar" "taffybar.css"

  let
    baseEndWidgets = [myTray, myNet, myMem, myCPU, myMpris]
    laptopEndWidgets = myBattery ++ baseEndWidgets
    simpleTaffyConfig =
      defaultSimpleTaffyConfig
        { startWidgets = [myWorkspaces]
        , centerWidgets = [myClock]
        , barPosition = Top
        , widgetSpacing = 0
        , barPadding = 0
        , barHeight = ScreenRatio (1 / 24)
        , cssPaths = [cssFiles]
        , endWidgets = laptopEndWidgets
        }

  startTaffybar $
    appendHook (void $ getTrayHost False) $
      withLogServer $
        withToggleServer $
          toTaffyConfig simpleTaffyConfig

setClassAndBoundingBoxes ::
  MonadIO m => Data.Text.Text -> Gtk.Widget -> m Gtk.Widget
setClassAndBoundingBoxes klass =
  buildContentsBox >=> flip widgetSetClassGI klass

deocrateWithSetClassAndBoxes ::
  MonadIO m => Data.Text.Text -> m Gtk.Widget -> m Gtk.Widget
deocrateWithSetClassAndBoxes klass builder =
  builder >>= setClassAndBoundingBoxes klass

makeCombinedWidget constructors = do
  widgets <- sequence constructors
  hbox <- Gtk.boxNew Gtk.OrientationHorizontal 0
  mapM_ (Gtk.containerAdd hbox) widgets
  Gtk.toWidget hbox

mkRGBA (r, g, b, a) = (r / 256, g / 256, b / 256, a / 256)

ctBlue = mkRGBA (122, 162, 247, 256)
ctRed = mkRGBA (247, 118, 142, 256)
ctYellow = mkRGBA (224, 175, 104, 256)

myGraphConfig =
  defaultGraphConfig
    { graphPadding = 0
    , graphBorderWidth = 0
    , graphWidth = 75
    , graphBackgroundColor = (0.0, 0.0, 0.0, 0.0)
    }

cpuCfg = myGraphConfig{graphDataColors = [ctRed], graphLabel = Just "Cpu"}

memCfg = myGraphConfig{graphDataColors = [ctBlue], graphLabel = Just "Mem"}

netCfg =
  myGraphConfig{graphDataColors = [ctYellow], graphLabel = Just "Net"}

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

enableLogger logger level = do
  getLogger logger >>= saveGlobalLogger . setLevel level

myCPU =
  deocrateWithSetClassAndBoxes "cpu" $
    pollingGraphNew cpuCfg 5 cpuCallback

myMem =
  deocrateWithSetClassAndBoxes "mem" $
    pollingGraphNew memCfg 5 memCallback

myNet =
  deocrateWithSetClassAndBoxes "net" $
    networkGraphNew netCfg Nothing

myWorkspaces =
  flip widgetSetClassGI "workspaces"
    =<< workspacesNew
      defaultWorkspacesConfig
        { minIcons = 1
        , getWindowIconPixbuf =
            scaledWindowIconPixbufGetter $
              getWindowIconPixbufFromChrome
                <|||> unscaledDefaultGetWindowIconPixbuf
                <|||> (\size _ -> lift $ loadPixbufByName size "application-default-icon")
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        , updateRateLimitMicroseconds = 100000
        , labelSetter = const (pure " ")
        , widgetBuilder = buildLabelOverlayController
        }

myClock =
  deocrateWithSetClassAndBoxes "clock" $
    textClockNewWith
      defaultClockConfig
        { clockUpdateStrategy = RoundedTargetInterval 60 0.0
        , clockFormatString = "\61463  %H:%M   \61555  %d/%m/%y"
        }

myTray =
  deocrateWithSetClassAndBoxes "tray" $
    sniTrayNewFromParams
      defaultTrayParams
        { trayRightClickAction = PopupMenu
        , trayLeftClickAction = Activate
        }

myMpris =
  mpris2NewWithConfig
    MPRIS2Config
      { mprisWidgetWrapper = deocrateWithSetClassAndBoxes "mpris" . return
      , updatePlayerWidget =
          simplePlayerWidget
            defaultPlayerConfig
              { setNowPlayingLabel = playingText 10 10
              }
      }

myBattery =
  [ deocrateWithSetClassAndBoxes "battery" $
      makeCombinedWidget
        [batteryIconNew, textBatteryNew "$percentage$%"]
  ]
