{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson

import Network.Wai.Handler.Warp (Port, run)

import qualified Data.ByteString.Lazy as B

import Warframing.Mod.Mod (makeNormalizedModMap)
import Warframing.Api (modApp)

data Config = Config { modsLocation :: FilePath
                     , port :: Port
                     } deriving Show

config :: Config
config = Config "./data/Mods.json" 8081

main :: IO ()
main = do
    putStrLn "trying some stuff..."

    mods <- B.readFile (modsLocation config)

    case decode mods of
        Just mods' -> do
            let mods'' = makeNormalizedModMap mods'
            run (port config) $ modApp mods''
        Nothing -> do
            putStrLn "failed JSON parse"

    putStrLn "something?"

