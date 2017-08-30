module Shell.Prelude (
  module Exports
  , loopN, break, continue, whileM_
  , findFirstPath
  , interactiveShell
  , outputConcurrentMsg, shell'
) where

import           Control.Lens              as Exports (makeClassy,
                                                       makeFieldsNoPrefix,
                                                       makeLenses, strict, view, _1, _2)
import           Control.Lens.Operators    as Exports hiding ((<.>))
import           Control.Monad.Trans.Maybe
import           Numeric.Natural           as Exports
import           Protolude                 as Exports hiding (break, die, (%), (<&>), Down)
import           System.Console.Concurrent (createProcessConcurrent,
                                            outputConcurrent,
                                            waitForProcessConcurrent)
import qualified System.Process            as Process
import qualified Turtle


-- | Give a list of file paths, find the first existing file
findFirstPath :: MonadIO io => [Text] -> io (Maybe Text)
findFirstPath paths =
  asum <$> for paths (\p -> do
    found <- Turtle.testfile (Turtle.fromText p)
    let path = if found then Just p else Nothing
    pure path)

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
  where
    go = ifM p (f >> go) (return ())

-- | Loop n times with the ability to `break` the loop
--   `Data.Foldable.asum` will strive for the first non empty value
loopN :: MonadIO m => Int -> MaybeT m () -> m (Maybe ())
loopN n = runMaybeT . asum . replicate n

-- | Exit point for `loopN`
break :: MonadIO m => MaybeT m ()
break = pure ()

continue :: MonadIO m => MaybeT m ()
continue = empty

-- | Run a command in the shell for interactive console processes
--   Used when running 'cicd console'
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
