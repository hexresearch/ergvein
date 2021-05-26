{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Data.IORef
import Data.List (uncons)
import Data.Maybe (fromJust, catMaybes)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program (requireProgram, arProgram)
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Types.BuildInfo
import Distribution.Types.HookedBuildInfo
import Distribution.Verbosity
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.JSON

import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Simple.Program.Ar as Ar
import qualified Distribution.Compat.Graph as Gr

import Debug.Trace

main = defaultMainWithHooks simpleUserHooks {
    preBuild = rustPreBuild
  , postBuild = rustPostBuild
  }

getHaskStaticLibrary :: LocalBuildInfo -> Maybe String
getHaskStaticLibrary LocalBuildInfo{..} = fmap fst . uncons $ catMaybes $ fmap isLib $ Gr.toList componentGraph
  where
    isLib c = case c of
      LibComponentLocalBuildInfo{..} -> Just $ unComponentId componentComponentId
      _ -> Nothing

rustPathRef :: IORef FilePath
rustPathRef = unsafePerformIO $ newIORef ""
{-# NOINLINE rustPathRef #-}

rustPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
rustPreBuild _ _ = do
  putStrLn "Building rust library..."
  rustLibPath <- buildRustLib
  let libName = drop 3 $ dropExtension $ takeFileName rustLibPath
  writeIORef rustPathRef rustLibPath
  pure (Just emptyBuildInfo {
    extraLibs = [ libName ]
  , extraLibDirs = [ takeDirectory rustLibPath ]
  }, [])

rustPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
rustPostBuild _ _ _ lbi = do
  rustLibPath <- readIORef rustPathRef
  putStrLn "Repacking archive..."
  haskLib <- maybe (fail "Cannot find library component of package") pure $ getHaskStaticLibrary lbi
  let haskLibPath = buildDir lbi </> ("libHS" <> haskLib <> ".a")
  let resultPath = buildDir lbi </> "result.a"
  let mriScript = unlines [
          "create " <> resultPath
        , "addlib " <> haskLibPath
        , "addlib " <> rustLibPath
        , "save"
        , "end" ]
  (ec, _, err) <- readProcessWithExitCode "ar" ["-M"] mriScript
  when (ec /= ExitSuccess) $ fail $ "Failed to repack archive: " <> err
  removeFile haskLibPath
  renameFile resultPath haskLibPath

buildRustLib :: IO FilePath
buildRustLib = do
  -- Ask rust to output path to resulted library
  setEnv "RUSTFLAGS" "--print native-static-libs"
  let cargoToml = "./rust/Cargo.toml"
      cargoArgs = [ "build"
                  , "--release"
                  , "--manifest-path=" ++ cargoToml
                  ]
      msgFormat = [ "--message-format=json" ]

  ec <- spawnProcess "cargo" cargoArgs >>= waitForProcess
  when (ec /= ExitSuccess) $ fail rustcErrMsg

  -- Run Cargo again to get the static library path
  jOuts <- readProcess "cargo" (cargoArgs ++ msgFormat) ""
  let jOut = last (init $ lines jOuts)
  case decode jOut of
    Error msg -> fail ("buildRustLib: " ++ msg)
    Ok jObj -> case lookup "filenames" (fromJSObject jObj) of
                 Just (JSArray [ JSString jStr ]) -> pure (fromJSString jStr)
                 _ -> fail ("cargoFinalizer: did not find one static library from output: " ++ show (fromJSObject jObj))

rustcErrMsg :: String
rustcErrMsg = "Rust source file associated with this module failed to compile"
