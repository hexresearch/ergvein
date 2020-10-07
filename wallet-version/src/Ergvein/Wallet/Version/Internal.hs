module Ergvein.Wallet.Version.Internal(
    Version(..)
  , embedVersion
  , makeVersionString
  ) where

import Data.Data
import Data.Maybe
import Data.Text (Text)
import Ergvein.Aeson
import Ergvein.Text (showt)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..), dataToExpQ)
import System.Environment (getEnvironment)
import Text.Read (readMaybe)

import qualified Data.Text as T

-- | Compile time version info that is embedded via TH. Collected from `GIT_HASH`,
-- `VERSION_TAG` env vars.
data Version = Version
  { versionHash   :: !(Maybe Text) -- ^ Commit hash
  , versionTag    :: !Text -- ^ Current version tag
  } deriving (Eq, Show, Data)

$(deriveJSON dropPrefixOptions ''Version)

-- | Lift text as expression in TH. See https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)

-- | Looks up a compile-time environment variable.
lookupCompileEnv :: String -> Q (Maybe Text)
lookupCompileEnv key = fmap (T.strip . T.pack) . lookup key <$> runIO getEnvironment

-- | Looks up a compile-time environment variable.
getCompileEnv :: String -> Text -> Q Text
getCompileEnv key defval = fromMaybe defval <$> lookupCompileEnv key

-- | Collect all envs in compile time
--
-- Returns expression of type Version
embedVersion :: Q Exp
embedVersion = (`sigE` [t| Version |]) . liftDataWithText =<< (Version
  <$> lookupCompileEnv "GIT_HASH"
  <*> getCompileEnv "VERSION_TAG" "development"
  )

-- | Make single string from version info
makeVersionString :: Version -> Text
makeVersionString Version{..} = case versionHash of
  Just h -> T.intercalate "-" [versionTag, h]
  Nothing -> versionTag
