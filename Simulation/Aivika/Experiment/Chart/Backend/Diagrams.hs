
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.Backend.Diagrams
-- Copyright  : Copyright (c) 2012-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- This module defines a renderer that uses the Chart-diagrams library
-- for rendering charts within simulation, i.e. without using Cairo,
-- which can be suitable for MS Windows.
--

module Simulation.Aivika.Experiment.Chart.Backend.Diagrams
       (DiagramsRenderer(..)) where

import System.FilePath
import Data.Map

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

-- | This renderer uses the Chart-diagrams library for rendering charts within simulation.
data DiagramsRenderer =
  DiagramsRenderer { rendererFileFormat :: FileFormat,
                     -- ^ It returns the file format used for saving the image.
                     rendererCustomFonts :: Map (String, FontSlant, FontWeight) FilePath
                     -- ^ It contains the custom fonts.
                   }

instance ChartRendering DiagramsRenderer where

  renderableChartExtension (DiagramsRenderer EPS _) = ".eps"
  renderableChartExtension (DiagramsRenderer SVG _) = ".svg"
  renderableChartExtension (DiagramsRenderer SVG_EMBEDDED _)  = ".svg"
  
  renderChart (DiagramsRenderer format fonts) (width, height) =
    renderableToFile (FileOptions (fromIntegral width, fromIntegral height) format fonts)
