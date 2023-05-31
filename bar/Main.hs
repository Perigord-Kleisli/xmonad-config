{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Default (def)
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph

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
      mem = pollingGraphNew memCfg 0.5 memCallback
      battery = textBatteryNew "$percentage$%"
      clock = textClockNewWith def
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      workspaces = workspacesNew def
      simpleConfig =
        def
          { startWidgets = [workspaces]
          , endWidgets = reverse [cpu, mem, battery, clock, sniTrayNew]
          }
  simpleTaffybar simpleConfig
