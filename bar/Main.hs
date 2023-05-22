{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Default (def)
import GI.Gtk (Orientation (OrientationHorizontal), boxNew)
import GI.Gtk qualified as GTK
import GI.Gtk.Objects qualified as Gtk
import GI.Gtk.Objects.Widget
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Hooks (getNetworkChan)
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Information.Memory

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]


memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

main :: IO ()
main = do
  let cpuCfg =
        def
          { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
          , graphLabel = Just "cpu"
          }
      memCfg =
        def
          { graphDataColors = [(0.129, 0.588, 0.953, 1)]
          , graphLabel = Just "mem"
          }
      clock = textClockNewWith def
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 0.5 memCallback
      battery = textBatteryNew "$percentage$%"
      workspaces = workspacesNew def
      simpleConfig =
        def
          { startWidgets = [workspaces]
          , endWidgets = reverse [cpu, mem, battery, clock, sniTrayNew]
          }
  simpleTaffybar simpleConfig
