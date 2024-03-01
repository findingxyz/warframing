{-# LANGUAGE OverloadedStrings #-}

module Warframing.Text (
    warframeText
  , normalizeSearch
) where

import Data.Char (isAlphaNum)

import Data.Text (Text)
import qualified Data.Text as T

warframeText :: Char -> Bool
warframeText c = T.elem c "-+<_>'% \n"
              || isAlphaNum c

normalizeSearch :: Text -> Text
normalizeSearch = T.filter warframeText . T.toLower

