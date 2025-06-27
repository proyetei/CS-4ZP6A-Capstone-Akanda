-- | HTML reports for a @panbench@ run.
module Panbench.HTML
  ( benchmarkHtml
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import Data.Aeson qualified as J
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT

import Graphics.Vega.VegaLite (VegaLite)
import Graphics.Vega.VegaLite qualified as VL

import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

import Panbench.Vega

encodeJsonUtf8 :: Value -> Text
encodeJsonUtf8 = LT.toStrict . LT.decodeUtf8 . J.encode

-- | Construct a benchmarking report from a JSON encoded data blob.
benchmarkHtml
  :: Text
  -> Text
  -- ^ Vega-lite source text.
  -> Text
  -- ^ Vega-embed source text
  -> Value
  -- ^ JSON data blob.
  -> Html
benchmarkHtml vega vegaLite vegaEmbed jsonData =
  H.docTypeHtml do
    H.head do
      H.title "Panbench"
      H.meta H.! A.charset "UTF-8"
      H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml vega
      H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml vegaLite
      H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml vegaEmbed
    H.body do
      -- [FIXME: Reed M, 24/06/2025] We are duplicating the data 3 times.
      benchmarkChart "userTime" $ VL.toVegaLite
        [ VL.datasets [("data", VL.dataFromJson jsonData [])]
        , VL.layer [userTimeLayer "data" 600 600]
        ]
      benchmarkChart "systemTime" $ VL.toVegaLite
        [ VL.datasets [("data", VL.dataFromJson jsonData [])]
        , VL.layer [systemTimeLayer "data" 600 600]
        ]
      benchmarkChart "maxRss" $ VL.toVegaLite
        [ VL.datasets [("data", VL.dataFromJson jsonData [])]
        , VL.layer [maxRssLayer "data" 600 600]
        ]

benchmarkChart
  :: Text
  -> VegaLite
  -> Html
benchmarkChart chartId vega = do
  H.div H.! A.id (H.toValue chartId) $ ""
  H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml $
    T.unlines
    [ "var spec" <> chartId <> " =" <> encodeJsonUtf8 (VL.fromVL vega) <> ";"
    , "vegaEmbed('#" <> chartId <> "', spec" <> chartId <> ")"
    , ".then((res) => {"
    , "});"
    ]
