-- | Idiomatic usage of Options for the project
--
module Shell.Options (
  -- * custom primitive
    optText
  , argText
  , arg
  , optRead
  , subcommand

  -- * re-export from Options.Applicative
  , Parser (..)
  , long
  , short
  , switch
  , help
  , metavar
  , auto
) where

import           Options.Applicative
import qualified Options.Applicative       as Opts
import qualified Options.Applicative.Types as Opts
import           Shell.Prelude hiding (option)

optText :: Mod OptionFields Text -> Parser Text
optText = opt Just

opt :: (Text -> Maybe a)
    -> Mod OptionFields a
    -> Parser a
opt argParse mod
 = option (argParseToReadM argParse) mod

-- | avoid conflict with semigroup option
optRead = option

argParseToReadM :: (Text -> Maybe a) -> ReadM a
argParseToReadM f = do
    s <- Opts.readerAsk
    case f (toS s) of
        Just a  -> return a
        Nothing -> Opts.readerAbort Opts.ShowHelpText

argText :: Mod ArgumentFields Text -> Parser Text
argText = arg Just

arg :: (Text -> Maybe a)
    -> Mod ArgumentFields a
    -> Parser a
arg argParse  mod
   = argument (argParseToReadM argParse) mod

subcommand name desc p =
    hsubparser (command name info <> metavar name)
  where
    info = Opts.info p (progDescDoc (Just desc))
