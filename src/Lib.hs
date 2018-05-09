{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( app
    ) where

import System.IO
import System.Environment
import qualified Configuration.Dotenv as Dotenv
import Configuration.Dotenv.Types (defaultConfig)
import Network.Wreq
import qualified Data.Text as T
import Control.Lens ((&), (^?), (.~))
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson.Lens

app :: IO ()
app = do
  hSetBuffering stdout NoBuffering
  Dotenv.loadFile defaultConfig

  token <- T.pack <$> getEnv "SLACK_API_TOKEN"

  let opts = defaults & param "token" .~ [token]
  r <- getWith opts "https://slack.com/api/rtm.connect"
  print $ r ^? responseBody . key "url" . _String
