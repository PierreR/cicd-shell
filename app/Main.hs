module Main where

import           Control.Lens.Operators
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Optional          (Optional (..))
import qualified Data.Optional          as Optional
import qualified Data.Text              as Text
import qualified System.Process         as Process hiding (FilePath)
import           Turtle

import           Option
import           PepCmd
import           Type

-- need to come from http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages.language-puppet.x86_64-linux
-- 14 nov 2016
nixpkgs = "12a057cbe07a0ee30b28b4edb39c0a453a4d0556"
-- nixpkgs = "b02e6cc70d3e458eccb99ea20c4746238ebf52c2"
userhome = "/home/vagrant"
projectDir = userhome </> "projects/cicd/shell"

user = "${SALTAPI_USER}"
user_pwd_file = userhome </> ".user_pwd"
pgUrl  =  "${PGSERVER_URL}/salt_result"
puppetdbUrl  = "\"${PUPPETDB_URL}\""

getStack :: Maybe Stack -> Shell Stack
getStack s = do
  ds <- input (userhome </> ".user_stack")
  return $ fromMaybe (lineToText ds) s

runCommand :: Text -> PepCmd -> Shell ExitCode
runCommand zone cmd =  do
  let
    nixfile z = z <> ".nix"
    nixcommand z = Text.unwords ["nix-shell", nixfile z]
    msg = cmd ^. cmdmsg
    pgr pwd = nixcommand zone <>  " --argstr user_pwd " <> pwd <> " -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/" <> nixpkgs <> ".tar.gz"
    pepcmd pwd = if Text.null (cmd^.cmdpep) then pgr pwd else pgr pwd <> " --command '" <> cmd^.cmdpep <> "'"

    initEnv z = do
      found <- testfile $ fromText (nixfile z)
      unless found $ procs "./set_nix.sh" [z] empty

  salt_pass <- lineToText <$> input user_pwd_file
  pushd projectDir
  initEnv zone
  unless (null msg) $ confirm (fromJust msg)
  -- liftIO $ print (pepcmd salt_pass)
  case cmd^.cmdjq of
    Default -> interactive (pepcmd salt_pass)
    Specific jq -> do
      -- liftIO $ print jq
      inshell (pepcmd salt_pass) empty & shell jq


-- prohibited options
run (Options zone (Data (Nothing, Arg Nothing Nothing Nothing s)))  = die "Running data on the whole stack is currently prohibited"

-- valid options
run (Options zone Console)                       = runCommand zone consoleCmd
run (Options zone Stats)                         = runCommand zone statCmd
run (Options zone (Facts (across, Arg r n g s))) = getStack s >>= runCommand zone . factCmd puppetdbUrl r n g across
run (Options zone (Ping (Arg r n g s)))          = getStack s >>= runCommand zone . pingCmd r n g
run (Options zone (Runpuppet (Arg r n g s )))    = getStack s >>= runCommand zone . runpuppetCmd r n g
run (Options zone (Sync (Arg r n g s)))          = getStack s >>= runCommand zone . syncCmd r n g
run (Options zone (Data (key, Arg r n g s)))     = getStack s >>= runCommand zone . dataCmd key r n g
run (Options zone (Orchestrate (cmd, s)))        = getStack s >>= runCommand zone . orchCmd cmd
run (Options zone (Du (Arg r n g s)))            = getStack s >>= runCommand zone . duCmd r n g
run (Options zone (Result (ResultNum n)))        = runCommand zone (resultCmd  user pgUrl Nothing (Just n))
run (Options zone (Result (ResultJob j )))       = runCommand zone (resultCmd user pgUrl (Just j) Nothing)


main :: IO ()
main = sh $
  options "CICD command line utility" parser >>= run

confirm msg = do
  echo $ msg <> "? (Y/N)"
  r <- readline
  case r of
    Just "Y" -> return ()
    _        -> die "Abort by the user"

interactive :: MonadIO io => Text -> io ExitCode
interactive c = do
    let
      cp = (Process.shell (Text.unpack c))
            { Process.std_in  = Process.Inherit
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            , Process.delegate_ctlc = True
            }
    (_, _, _, ph) <- liftIO $ Process.createProcess cp
    liftIO $ Process.waitForProcess ph
