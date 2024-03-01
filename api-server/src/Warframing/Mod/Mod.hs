{-# LANGUAGE OverloadedStrings #-}

module Warframing.Mod.Mod (
    ModMap
  , Mod (..)
  , NormalMod (..)
  , makeNormalizedModMap
) where

import GHC.Generics

import Data.Aeson

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

newtype LevelStat = LevelStat {
    stats :: [Text]
} deriving (Show, Generic)

instance ToJSON LevelStat
instance FromJSON LevelStat

data Drop = Drop {
    chance :: Float
  , location :: Text
} deriving (Show, Generic)

instance ToJSON Drop
instance FromJSON Drop where
    parseJSON = withObject "Drop" $ \v -> Drop
        <$> v .: "chance"
        <*> v .: "location"

data Introduced = Introduced {
    name :: Text
  , url :: Text
  , aliases :: [Text]
  , parent :: Text
  , date :: Text
} deriving (Show, Generic)

instance ToJSON Introduced
instance FromJSON Introduced

data Mod = Mod {
    uniqueName :: Text
  , modName :: Text
  , modType :: Text
  , baseDrain :: Maybe Int
  , compatName :: Maybe Text
  , description :: Maybe Text
  , drops :: Maybe [Drop]
  , excludeFromCodex :: Maybe Bool
  , fusionLimit :: Maybe Int
  , introduced :: Maybe Introduced
  , isAugment :: Maybe Bool
  , isExilus :: Maybe Bool
  , isPrime :: Bool
  , isUtility :: Maybe Bool
  , levelStats :: Maybe [LevelStat]
  , polarity :: Maybe Text
  , rarity :: Maybe Text
  , tradeable :: Maybe Bool
  , transmutable :: Maybe Bool
} deriving (Show, Generic)

instance ToJSON Mod
instance FromJSON Mod where
    parseJSON = withObject "Mod" $ \v -> Mod
        <$> v .:  "uniqueName"
        <*> v .:  "name"
        <*> v .:  "type"
        <*> v .:? "baseDrain"
        <*> v .:? "compatName"
        <*> v .:? "description"
        <*> v .:? "drops"
        <*> v .:? "excludeFromCodex"
        <*> v .:? "fusionLimit"
        <*> v .:? "introduced"
        <*> v .:? "isAugment"
        <*> v .:? "isExilus"
        <*> v .:  "isPrime"
        <*> v .:? "isUtility"
        <*> v .:? "levelStats"
        <*> v .:? "polarity"
        <*> v .:? "rarity"
        <*> v .:? "tradeable"
        <*> v .:? "transmutable"

data NormalMod = NormalMod {
    normalUniqueName :: Text
  , normalName :: Text
  , normalModType :: Text
  , normalCompatName :: Maybe Text
  , normalDescription :: Maybe Text
  , normalLevelStats :: Maybe Text
  , normalPolarity :: Maybe Text
  , normalRarity :: Maybe Text
} deriving (Show)

type ModMap = HM.HashMap Text (NormalMod, Mod)

normalizeMod :: Text -> Text
normalizeMod = T.toLower

levelStatConcat :: [LevelStat] -> Text
levelStatConcat = T.intercalate "|" . map (T.intercalate "\n") . foldr (\(LevelStat x) acc -> x : acc) []

makeNormalizedModMap :: [Mod] -> ModMap
makeNormalizedModMap =
    foldr (\mod'@(Mod
        uniqueName'
        name'
        modType'
        baseDrain'
        compatName'
        description'
        drops'
        excludeFromCodex'
        fusionLimit'
        introduced'
        isAugment'
        isExilus'
        isPrime'
        isUtility'
        levelStats'
        polarity'
        rarity'
        tradeable'
        transmutable') acc -> if excludeFromCodex' == Just True
                              then acc
                              else HM.insert uniqueName' (NormalMod
                                       (normalizeMod uniqueName')
                                       (normalizeMod name')
                                       (normalizeMod modType')
                                       (if compatName' == Nothing then Nothing else normalizeMod <$> compatName')
                                       (normalizeMod <$> description')
                                       (normalizeMod . levelStatConcat <$> levelStats')
                                       (normalizeMod <$> polarity')
                                       (normalizeMod <$> rarity'), mod') acc) HM.empty

