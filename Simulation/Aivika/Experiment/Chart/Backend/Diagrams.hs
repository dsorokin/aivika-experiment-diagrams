
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.Backend.Diagrams
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- This module defines a renderer that uses the Chart-diagrams library
-- for rendering charts within simulation, i.e. without using Cairo,
-- which can be suitable for MS Windows.
--

module Simulation.Aivika.Experiment.Chart.Backend.Diagrams
       (DiagramsRenderer(..)) where

import System.FilePath

import Data.Map
import Data.Colour
import Data.Colour.Names

import Control.Lens

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

-- | This renderer uses the Chart-diagrams library for rendering charts within simulation.
data DiagramsRenderer =
  DiagramsRenderer { rendererFileFormat :: FileFormat,
                     -- ^ It returns the file format used for saving the image.
                     rendererCustomFonts :: IO (FontSelector Double)
                     -- ^ It contains the custom fonts.
                   }

instance ChartRendering DiagramsRenderer where

  renderableChartExtension (DiagramsRenderer EPS _) = ".eps"
  renderableChartExtension (DiagramsRenderer SVG _) = ".svg"
  renderableChartExtension (DiagramsRenderer SVG_EMBEDDED _)  = ".svg"
  
  renderChart (DiagramsRenderer format fonts) (width, height) =
    renderableToFile (FileOptions (fromIntegral width, fromIntegral height) format fonts)

  renderingLayout (DiagramsRenderer _ _) = defaultLayout
  renderingLayoutLR (DiagramsRenderer _ _) = defaultLayoutLR

-- | Default font style.
defaultFontStyle :: FontStyle
defaultFontStyle =
  FontStyle "serif" 16 FontSlantNormal FontWeightNormal (opaque black) 

-- | Default title font style.
defaultTitleFontStyle :: FontStyle
defaultTitleFontStyle =
  FontStyle "serif" 20 FontSlantNormal FontWeightBold (opaque black) 

-- | The default layout.
defaultLayoutLR :: LayoutLR Double Double Double -> LayoutLR Double Double Double
defaultLayoutLR layoutlr =
  layoutlr_title_style .~ defaultTitleFontStyle $
  layoutlr_all_font_styles .~ defaultFontStyle $
  layoutlr

-- | The default layout.
defaultLayout :: Layout Double Double -> Layout Double Double
defaultLayout layout =
  layout_title_style .~ defaultTitleFontStyle $
  layout_all_font_styles .~ defaultFontStyle $
  layout
