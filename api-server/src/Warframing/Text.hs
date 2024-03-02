{-# LANGUAGE OverloadedStrings #-}

module Warframing.Text (
    warframeText
  , _warframeText
  , normalizeSearch
) where

import Data.Char (isAlphaNum)

import Data.Text (Text)
import qualified Data.Text as T

warframeText :: Char -> Bool
warframeText c = T.elem c " \n"
              || _warframeText c

_warframeText :: Char -> Bool
_warframeText c = T.elem c "-+'\"%"
               || isAlphaNum c

normalizeSearch :: Text -> Text
normalizeSearch = T.replace "+" "\\+" . T.filter warframeText . T.toLower

