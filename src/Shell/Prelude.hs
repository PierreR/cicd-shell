module Shell.Prelude (
  module Exports
  , loopN, break, continue
  , interactiveShell
) where

import           Control.Lens                 as Exports (makeClassy,
                                                          makeFieldsNoPrefix,
                                                          makeLenses, strict,
                                                          view)
import           Control.Lens.Operators       as Exports hiding ((<.>))
import           Control.Monad.Trans.Maybe
import           Data.Optional                as Exports (Optional, Optional (Default, Specific))
import           Numeric.Natural              as Exports
import           Protolude                    as Exports hiding (break, die,
                                                          (%))
import qualified System.Process               as Process

continue :: MonadIO m => MaybeT m ()
continue = empty

-- asum will strive for the first non empty value
-- that's why returning one would break the loop
-- As a note, compare this with forever ... which would have the opposite behavior
-- `runMaybeT . forever` would break whenever empty is encountered.
break :: MonadIO m => MaybeT m ()
break = pure ()

-- | loop n times with the ability to break the loop
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
