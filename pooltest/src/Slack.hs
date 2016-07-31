module Slack where

import           Control.Monad
import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import qualified Network.Wreq        as W
import           System.Timeout

import           Protocol

webhookURL ∷ String
webhookURL = "https://hooks.slack.com/services/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"

opts ∷ W.Options
opts = W.defaults

send sub msg = void $ timeout (round 1e6) $ do
  _ <- W.postWith opts webhookURL $ A.Object $ M.fromList [("text", A.String msg), ("username", A.String $ user sub), ("icon_emoji", A.String $ emoji sub), ("parse", A.String "full")]
  return ()

emoji ∷ Sub → T.Text
emoji Thor = ":thor:"
emoji Loki = ":loki:"

user ∷ Sub → T.Text
user Thor = "Thor"
user Loki = "Loki"
