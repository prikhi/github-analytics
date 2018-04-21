{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Clay
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)

import qualified Data.ByteString as B

import Prelude hiding (div)


renderedCss :: B.ByteString
renderedCss = encodeUtf8 . toStrict . render $ do
    star # byClass "hidden" ? display none
    body ? height (pct 100)
    div # byId "body-wrapper" ? do
        display flex
        flexDirection column
        justifyContent center
        alignItems center
        height $ pct 100
