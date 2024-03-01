{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Warframing.Api (
    modApp
) where

import Servant
import Data.Attoparsec.Text
import Data.Text (Text)

import qualified Data.HashMap.Strict as HM

import Warframing.Mod.Mod
import Warframing.Search.Parser (queries)
import Warframing.Mod.Search (modSearch)

type ModAPI = "api" :> "mods" :> "search" :> QueryParam "q" Text :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" Text] [Mod])

modAPI :: Proxy ModAPI
modAPI = Proxy

modApp :: ModMap -> Application
modApp = serve modAPI . modServer

modServer :: ModMap -> Server ModAPI
modServer mm = modServe
    where modServe mquery = return $ addHeader "*" $ case mquery of
              Nothing -> map snd . HM.elems $ mm
              Just q  -> case parseOnly queries q of
                  Right q' -> map snd . HM.elems . modSearch q' $ mm
                  Left e -> error ("whoops: " ++ e)
