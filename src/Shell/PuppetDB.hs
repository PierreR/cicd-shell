{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Shell.PuppetDB (
  getFacts
) where

import           Data.Default.Class
import           Network.HTTP.Req
import qualified Shell.Config       as Config
import           Shell.Prelude
import NeatInterpolation

getFacts :: MonadIO m => Text -> m LByteString
getFacts n = runReq def $ do
  let param_val = [text| ["=", "certname", "$n"] |] :: Text
      param = "query" =: param_val
  r <- req POST
    (http Config.puppetdbServer /: "pdb" /: "query" /: "v4" /: "facts")
    (ReqBodyUrlEnc param)
    lbsResponse
    $ (port Config.puppetdbPort)
  pure $ responseBody r
