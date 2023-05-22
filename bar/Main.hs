{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default (def)
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Network (getDeviceSamples)
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main :: IO ()
main = do
  let cpuCfg =
        def
          { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
          , graphLabel = Just "cpu"
          }
      netCfg = def{graphLabel = Just "net"}
      clock = textClockNewWith def
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      network = networkGraphNew netCfg Nothing
      workspaces = workspacesNew def
      simpleConfig =
        def
          { startWidgets = [workspaces]
          , endWidgets = [clock, sniTrayNew, cpu, network]
          }
  simpleTaffybar simpleConfig
