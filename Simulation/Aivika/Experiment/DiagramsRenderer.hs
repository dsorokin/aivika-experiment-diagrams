
-- |
-- Module     : Simulation.Aivika.Experiment.DiagramsRenderer
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- This module defines a renderer that uses the Chart-diagrams library
-- for rendering charts with simulation results, i.e. without using Cairo,
-- which can be suitable for MS Windows.
--

module Simulation.Aivika.Experiment.DiagramsRenderer
       (DiagramsRenderer(..)) where

import System.FilePath
import Data.Map

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

-- | This renderer uses the Chart-diagrams library for rendering charts with simulation results.
data DiagramsRenderer =
  DiagramsRenderer { rendererFileFormat :: FileFormat,
                     -- ^ It returns the file format used for saving the image.
                     rendererCustomFonts :: Map (String, FontSlant, FontWeight) FilePath
                     -- ^ It contains the custom fonts.
                   }

instance FileRenderer DiagramsRenderer

instance ChartRenderer DiagramsRenderer where

  renderableFileExtension (DiagramsRenderer EPS _) = ".eps"
  renderableFileExtension (DiagramsRenderer SVG _) = ".svg"
  renderableFileExtension (DiagramsRenderer SVG_EMBEDDED _)  = ".svg"
  
  renderChart (DiagramsRenderer format fonts) (width, height) =
    flip $ renderableToFile (FileOptions (fromIntegral width, fromIntegral height) format fonts)
