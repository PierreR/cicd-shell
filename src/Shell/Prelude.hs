-- | General project specific utilities.
module Shell.Prelude (
  module Exports
  , loopN, break, continue, whileM_
  , findFirstPath
  , interactiveShell
  , outputConcurrentMsg, shell'
  , touchFile
) where

import           Control.Lens              as Exports (at, makeClassy,
                                                       makeFieldsNoPrefix,
                                                       makeLenses, strict, view,
                                                       _1, _2, _Just)
import           Control.Lens.Operators    as Exports hiding ((<.>))
import           Control.Monad.Trans.Maybe
import           System.FilePath           as Exports ((</>))
import           Numeric.Natural           as Exports
import           Protolude                 as Exports hiding (Down, break,
                                                       (%), (<&>))

import           System.Console.Concurrent (createProcessConcurrent,
                                            outputConcurrent,
                                            waitForProcessConcurrent)
import qualified System.Process            as Process
import qualified System.Directory          as Directory
import qualified System.IO


touchFile = System.IO.appendFile ""

-- | Given a list of file paths, find the first existing file.
findFirstPath :: MonadIO io => [FilePath] -> io (Maybe FilePath)
findFirstPath paths =
  asum <$> for paths (\p -> do
    found <- liftIO $ Directory.doesFileExist p
    let path = if found then Just p else Nothing
    pure path)

-- | Play a computation for its side-effect as long as a predate holds
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
  where
    go = ifM p (f *> go) (pure ())

-- | Loop n times with the ability to `break` the loop.
--
--   `Data.Foldable.asum` will strive for the first non empty value.
loopN :: MonadIO m => Int -> MaybeT m () -> m (Maybe ())
loopN n = runMaybeT . asum . replicate n

-- | Exit point for `loopN`
break :: MonadIO m => MaybeT m ()
break = pure ()

continue :: MonadIO m => MaybeT m ()
continue = empty

-- | Wrap the opening of a interactive console (`cicd ZONE console`).
interactiveShell :: MonadIO io
            => Text
            -- ^ Command
            -> io ExitCode
interactiveShell c = do
    let
      cp = (Process.shell (toS c))
            { Process.std_in  = Process.Inherit
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            , Process.delegate_ctlc = True
            }
    (_, _, _, ph) <- liftIO $ Process.createProcess cp
    liftIO $ Process.waitForProcess ph


-- | Run a shell process within an interactive console
shell' :: Text -> IO ExitCode
shell' cmd = do
  (_, _, _, b) <- createProcessConcurrent $ Process.shell (toS cmd)
  waitForProcessConcurrent b

outputConcurrentMsg :: Text -> IO ()
outputConcurrentMsg = outputConcurrent
