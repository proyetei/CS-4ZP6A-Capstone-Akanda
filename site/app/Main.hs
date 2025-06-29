module Main where

import Data.Aeson qualified as JSON
import Data.Set qualified as Set
import Data.Text.IO qualified as T

import Development.Shake

import Text.Blaze.Html.Renderer.Utf8 qualified as H

import Panbench.Shake.File
import Panbench.Shake.Benchmark
import Panbench.HTML
import Panbench

import Panbench.Shake.Matrix
import Panbench.Shake.Lang

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  benchmarkRules
  generatorRules
  benchmarkMatrixRules

  phony "clean" do
    removeFilesAfter "_build" ["agda/*", "lean/*", "idris2/*", "rocq/*", "*.html"]

  "_build/index.html" %> \out -> do
    need [vegaSrc, vegaLiteSrc, vegaEmbedSrc]
    vegaJs <- liftIO $ T.readFile vegaSrc
    vegaLiteJs <- liftIO $ T.readFile vegaLiteSrc
    vegaEmbedJs <- liftIO $ T.readFile vegaEmbedSrc
    stats <- needBenchmarkMatrix (BenchmarkMatrix (Set.fromList [Agda, Idris, Lean, Rocq]) [2^n | n <- [(0 :: Int)..10]] "LetExample")
    writeBinaryFileChanged out
      $ H.renderHtml
      $ benchmarkHtml vegaJs vegaLiteJs vegaEmbedJs
      $ JSON.toJSON stats
    where
      vegaSrc = "web/vega@5.10.js"
      vegaLiteSrc = "web/vega-lite@4.7.0.js"
      vegaEmbedSrc = "web/vega-embed@6.3.2.js"
