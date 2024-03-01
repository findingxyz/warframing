{-# LANGUAGE OverloadedStrings #-}

module Warframing.Search.Parser (
    queries
  , relate
  , Query (..)
  , Relation
) where

import Data.Char (isAlphaNum)
import Control.Applicative ((<|>))

import Data.Text (Text)

import Data.Attoparsec.Text

import Warframing.Text (warframeText)

data Relation = LT' | LTEQ | EQ' | GTEQ | GT' | Bill
    deriving Show

toRelation :: Text -> Relation
toRelation "<" = LT'
toRelation "<=" = LTEQ
toRelation "=" = EQ'
toRelation ">=" = GTEQ
toRelation ">" = GT'
toRelation ":" = EQ'
toRelation _ = Bill --error "not satisfied"

relate :: Ord a => Relation -> (a -> a -> Bool)
relate LT' = (<)
relate LTEQ = (<=)
relate EQ' = (==)
relate GTEQ = (>=)
relate GT' = (>)
relate Bill = error "bill"

data Query = Untagged Text | Tagged Text Relation Text
    deriving Show

untagged :: Parser Query
untagged = Untagged <$> term
    <?> "untagged"

tagged :: Parser Query
tagged = Tagged <$> word <*> (toRelation <$> takeWhile1 (not . (\c -> warframeText c || (c == '"')))) <*> term
    <?> "tagged"

word :: Parser Text
word = takeWhile1 isAlphaNum
    <?> "word"

literal :: Parser Text
literal = char '\"' *> takeTill (=='\"') <* char '\"'
    <?> "literal"

term :: Parser Text
term = literal <|> word
    <?> "term"

query :: Parser Query
query = tagged <|> untagged
    <?> "query"

queries :: Parser [Query]
queries = query `sepBy` char ' '
    <?> "queries"
