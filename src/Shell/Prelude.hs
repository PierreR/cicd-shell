module Shell.Prelude (
  module Exports
  , loopN, break, continue
  , interactiveShell
  , outputConcurrentMsg, shell'
) where

import           Control.Lens              as Exports (makeClassy,
                                                       makeFieldsNoPrefix,
                                                       makeLenses, strict, view)
import           Control.Lens.Operators    as Exports hiding ((<.>))
import           Control.Monad.Trans.Maybe
import           Numeric.Natural           as Exports
import           Protolude                 as Exports hiding (break, die, (%), (<&>))
import           System.Console.Concurrent (createProcessConcurrent,
                                            outputConcurrent,
                                            waitForProcessConcurrent)
import qualified System.Process            as Process

continue :: MonadIO m => MaybeT m ()
continue = empty

-- | Exit point for `loopN`
break :: MonadIO m => MaybeT m ()
break = pure ()

-- | Loop n times with the ability to `break` the loop
--   `Data.Foldable.asum` will strive for the first non empty value
loopN :: MonadIO m => Int -> MaybeT m () -> m (Maybe ())
loopN n = runMaybeT . asum . replicate n

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


shell' :: Text -> IO ExitCode
shell' cmd = do
  (_, _, _, b) <- createProcessConcurrent $ Process.shell (toS cmd)
  waitForProcessConcurrent b


-- shell' :: Text -> Turtle.Shell(Turtle.Line) -> IO ExitCode
-- shell' cmd s = do
--   let open = do
--         (Just inh, Nothing, Nothing, ph) <- createProcessConcurrent $ (Process.shell (toS cmd))
--                                                                           { Process.std_in  = Process.CreatePipe
--                                                                           , Process.std_out = Process.Inherit
--                                                                           , Process.std_err = Process.Inherit
--                                                                           }
--         return (inh, ph)
--       handle (inh, ph) = do
--         Text.hPutStrLn inh s
--         -- Turtle.outhandle inh s
--         waitForProcessConcurrent ph
--       close (inh, ph) = do
--         IO.hClose inh
--         -- Process.terminateProcess ph
--   bracket open close handle
  -- let open = do
  --       (Nothing, Just south, Nothing, ph) <- createProcessConcurrent $ Process.shell (toS cmd)
  --       void $ waitForProcessConcurrent ph
  --       return (south, ph)
  --     close (south, ph) = do
  --       IO.hClose south
  --       -- Process.terminateProcess ph
  -- bracket open close $ \(south, ph) -> do
  --   Text.hGetContents south

outputConcurrentMsg :: Text -> IO ()
outputConcurrentMsg = outputConcurrent
