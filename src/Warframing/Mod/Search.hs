{-# LANGUAGE OverloadedStrings #-}

module Warframing.Mod.Search (
    modSearch
) where

import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.HashMap.Strict as HM

import Text.Regex.TDFA ((=~))

import Warframing.Mod.Mod
import Warframing.Search.Parser
import Warframing.Text

data Rarity = Common | Uncommon | Rare | Legendary
    deriving (Ord, Eq, Show)

toRarity :: Text -> Maybe Rarity
toRarity "common" = Just Common
toRarity "c" = Just Common
toRarity "uncommon" = Just Uncommon
toRarity "u" = Just Uncommon
toRarity "rare" = Just Rare
toRarity "r" = Just Rare
toRarity "legendary" = Just Legendary
toRarity "l" = Just Legendary
toRarity _ = Nothing

type Search = Text -> ModMap -> ModMap

isSearch :: Search
isSearch q = HM.filter (\(normMod, mod') -> case q of
    "exilus" -> case (isExilus mod', isUtility mod') of
                    (Just True, _) -> True
                    (_, Just True) -> True
                    _ -> False
    _ -> False)

uniqueSearch :: Search
uniqueSearch q = HM.filter (\(x, _) -> normalUniqueName x =~ q)

nameSearch :: Search
nameSearch q = HM.filter (\(x, _) -> normalName x =~ q)

typeSearch :: Search
typeSearch q = HM.filter (\(x, _) -> normalModType x =~ q)

compatNameSearch :: Search
compatNameSearch q = HM.filter (\(x, _) ->
    case normalCompatName x of
        Just x' -> x' =~ q
        Nothing -> False)

levelStatSearch :: Search
levelStatSearch q = HM.filter (\(x, _) ->
    case normalLevelStats x of
        Just x' -> x' =~ q
        Nothing -> False)

polaritySearch :: Search
polaritySearch q = HM.filter (\(x, _) ->
    case normalPolarity x of
        Just x' -> x' =~ q
        Nothing -> False)

raritySearch :: Relation -> Search
raritySearch r q = HM.filter (\(x, _) ->
    case normalRarity x of
        --Just x' -> x' =~ q
        Just x' -> case (toRarity x', toRarity q) of
                       (Just x'', Just q') -> let r' = relate r in x'' `r'` q'
                       _ -> False
        Nothing -> False)

minCapacitySearch :: Relation -> Search
minCapacitySearch r q = HM.filter (\(_, x) ->
    case baseDrain x of
        Just x' -> let r' = relate r in
                       case TR.decimal q of
                           Right (d, _) -> x' `r'` d
                           _ -> False
        Nothing -> False)

maxCapacitySearch :: Relation -> Search
maxCapacitySearch r q = HM.filter (\(_, x) ->
    case (baseDrain x, fusionLimit x) of
        (Just base, Just maxRank) ->
            let r' = relate r     in
                case TR.decimal q of
                    Right (d, _) -> (if base < 0 then base - maxRank else base + maxRank) `r'` d
                    _            -> False
        (_, _) -> False)

compatSearch :: Search
compatSearch q = HM.filter (\(x, _) ->
    case normalCompatName x of
        Just x' -> x' =~ q
        Nothing -> False)


noSearch :: Search
noSearch _ _ = HM.empty

modSearch :: [Query] -> ModMap -> ModMap
modSearch qs mm = foldr whatSearch mm qs
    where whatSearch (Untagged q) = nameSearch $ normalizeSearch q
          whatSearch (Tagged t r q) = (case t of
              "t" -> typeSearch
              "type" -> typeSearch
              "p" -> polaritySearch
              "polarity" -> polaritySearch
              "d" -> levelStatSearch
              "description" -> levelStatSearch
              "r" -> raritySearch r
              "rarity" -> raritySearch r
              "c" -> minCapacitySearch r
              "capacity" -> minCapacitySearch r
              "m" -> maxCapacitySearch r
              "maxcapacity" -> maxCapacitySearch r
              "f" -> compatSearch
              "fits" -> compatSearch
              "u" -> uniqueSearch
              "unique" -> uniqueSearch
              "is" -> isSearch
              _ -> noSearch) $ normalizeSearch q
