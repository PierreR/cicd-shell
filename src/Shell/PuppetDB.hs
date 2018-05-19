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
  let queryval = [text| ["=", "certname", "$n"] |] :: Text
      queryparam = "query" =: queryval
  r <- req GET
    (http Config.puppetdbServer /: "pdb" /: "query" /: "v4" /: "facts")
    NoReqBody
    lbsResponse
    $ (port Config.puppetdbPort)  <> queryparam
  pure $ responseBody r
