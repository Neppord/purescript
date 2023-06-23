module Test.TestUtils where

import Prelude

import Data.Array (concat, filter)
import Data.Maybe (Maybe(..), isJust)
import Data.String.CodeUnits (stripSuffix)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (Stats, isDirectory)
import Node.FS.Sync (readTextFile, readdir, stat, writeTextFile)
import Node.Path (FilePath, relative)
import Node.Process (lookupEnv)
import Test.Spec.Assertions (fail, shouldEqual)
import Node.Path (concat) as Path
import Node.FS.Aff (readTextFile, writeTextFile) as Aff
import Test.Spec (Spec)
import Debug (spy)

{-
import Language.PureScript as P
import Language.PureScript.CST as CST
import Language.PureScript.AST as AST
import Language.PureScript.Names as N
import Language.PureScript.Interactive.IO (findNodeProcess)
-}

{-
-- |
-- Fetches code necessary to run the tests with. The resulting support code
-- should then be checked in, so that npm/bower etc is not required to run the
-- tests.
--
-- Simply rerun this (via ghci is probably easiest) when the support code needs
-- updating.
--
updateSupportCode :: IO ()
updateSupportCode = withCurrentDirectory "tests/support" do
  let lastUpdatedFile = ".last_updated"
  skipUpdate <- fmap isJust >>> runMaybeT $ do
    -- We skip the update if: `.last_updated` exists,
    lastUpdated <- MaybeT $ getModificationTimeMaybe lastUpdatedFile

    -- ... and it was modified less than a day ago (no particular reason why
    -- "one day" specifically),
    now <- lift getCurrentTime
    guard $ now `diffUTCTime` lastUpdated < nominalDay

    -- ... and the needed directories exist,
    contents <- lift $ listDirectory "."
    guard $ "node_modules" `elem` contents && "bower_components" `elem` contents

    -- ... and everything else in `tests/support` is at least as old as
    -- `.last_updated`.
    modTimes <- lift $ traverse getModificationTime >>> filter (_ /= lastUpdatedFile) $ contents
    guard $ all (_ <= lastUpdated) modTimes

    pure unit

  unless skipUpdate $ do
    heading "Updating support code"
    callCommand "npm install"
    -- bower uses shebang "/usr/bin/env node", but we might have nodejs
    node <- either cannotFindNode pure =<< findNodeProcess
    -- Sometimes we run as a root (e.g. in simple docker containers)
    -- And we are non-interactive: https://github.com/bower/bower/issues/1162
    callProcess node ["node_modules/bower/bin/bower", "--allow-root", "install", "--config.interactive=false"]
    writeFile lastUpdatedFile ""
  where
  cannotFindNode :: String -> IO a
  cannotFindNode message = do
    hPutStrLn stderr message
    exitFailure

  getModificationTimeMaybe :: FilePath -> IO (Maybe UTCTime)
  getModificationTimeMaybe f = catch (Just <$> getModificationTime f) $ case _ of
    e | isDoesNotExistError e -> pure Nothing
      | otherwise             -> throw e

  heading msg = do
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " <> msg
    putStrLn $ replicate 79 '#'
    putStrLn ""

readInput :: Array FilePath -> IO Array (FilePath /\ T.Text)
readInput inputFiles = forM inputFiles $ \inputFile -> do
  text <- readUTF8FileT inputFile
  return (inputFile /\ text)

-- |
-- The support modules that should be cached between test cases, to avoid
-- excessive rebuilding.
--
getSupportModuleTuples :: IO Array (FilePath /\ P.Module)
getSupportModuleTuples = do
  cd <- getCurrentDirectory
  let supportDir = cd </> "tests" </> "support"
  psciFiles <- Glob.globDir1 (Glob.compile "**/*.purs") (supportDir </> "psci")
  libraries <- Glob.globDir1 (Glob.compile "purescript-*/src/**/*.purs") (supportDir </> "bower_components")
  let pursFiles = psciFiles <> libraries
  fileContents <- readInput pursFiles
  modules <- runExceptT $ ExceptT >>> return $ CST.parseFromFiles id fileContents
  case modules of
    Right ms -> return (fmap (fmap snd) ms)
    Left errs -> fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)

getSupportModuleNames :: IO Array T.Text
getSupportModuleNames = sort >>> map (P.runModuleName >>> P.getModuleName >>> snd) <$> getSupportModuleTuples

pushd :: forall a. FilePath -> IO a -> IO a
pushd dir act = do
  original <- getCurrentDirectory
  setCurrentDirectory dir
  result <- try act :: IO (Either IOException a)
  setCurrentDirectory original
  either throwIO return result


createOutputFile :: FilePath -> IO Handle
createOutputFile logfileName = do
  tmp <- getTemporaryDirectory
  createDirectoryIfMissing False (tmp </> logpath)
  openFile (tmp </> logpath </> logfileName) WriteMode

data SupportModules = SupportModules
  { supportModules :: Array P.Module
  , supportExterns :: Array P.ExternsFile
  , supportForeigns :: M.Map P.ModuleName FilePath
  }

setupSupportModules :: IO SupportModules
setupSupportModules = do
  ms <- getSupportModuleTuples
  let modules = map snd ms
  supportExterns <- runExceptT $ do
    foreigns <- inferForeignModules ms
    externs <- ExceptT >>> fmap fst >>> runTest $ P.make (makeActions modules foreigns) (CST.pureResult <$> modules)
    return (externs /\ foreigns)
  case supportExterns of
    Left errs -> fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
    Right (externs /\ foreigns) -> return $ SupportModules modules externs foreigns
-}

endsWith :: String -> String -> Boolean
endsWith thing p = stripSuffix (Pattern thing) p /= Nothing

getPursFiles :: FilePath -> Aff (Array FilePath)
getPursFiles path = do
  stats <- liftEffect $ stat path
  if stats # isDirectory then do
    files <- liftEffect $ readdir path
    let
      pursFiles = files
        # filter (endsWith ".purs")
        <#> \f -> Path.concat [ path, f ]
    rest <- for (files <#> \f -> Path.concat [ path, f ]) getPursFiles
    pure $ pursFiles <> concat rest
  else pure []

getTestFiles :: FilePath -> Aff (Array FilePath)
getTestFiles testDir = do
  let dir = Path.concat [ "tests", "purs", testDir ]
  getPursFiles dir

goldenVsString :: String -> String -> Aff Unit
goldenVsString goldenFile actual = do
  accept <- liftEffect (isJust <$> lookupEnv "HSPEC_ACCEPT")
  ci <- liftEffect (isJust <$> lookupEnv "CI")
  expected <- Aff.readTextFile UTF8 goldenFile
  if not ci && accept then do
    Aff.writeTextFile UTF8 goldenFile actual
  else do
    if expected == actual then pure unit
    else shouldEqual expected actual
