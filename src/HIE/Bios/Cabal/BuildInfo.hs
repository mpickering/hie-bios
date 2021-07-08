{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module HIE.Bios.Cabal.BuildInfo where

import Data.Aeson
import GHC.Generics

decodeBuildInfoFile :: FilePath -> IO BuildInfo
decodeBuildInfoFile fp = do
    eitherDecodeFileStrict fp
     >>= \case
        Left err -> fail $ "Could not parse show-build-info file: " ++ err
        Right buildInfos -> return buildInfos

data BuildInfo = BuildInfo
    { cabalVersion :: String
    , compiler :: CompilerInfo
    , components :: [ComponentInfo]
    } deriving (Generic, Show)

data CompilerInfo = CompilerInfo
    { flavour :: String
    , compilerId :: String
    , path :: String
    } deriving (Generic, Show)

data ComponentInfo = ComponentInfo
    { componentType :: String
    , componentName :: String
    , componentUnitId :: String
    , componentCompilerArgs :: [String]
    , componentModules :: [String]
    , componentSrcFiles :: [FilePath]
    , componentHsSrcDirs :: [FilePath]
    , componentSrcDir :: FilePath
    , componentCabalFile :: Maybe FilePath
    } deriving (Generic, Show)

instance ToJSON BuildInfo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON BuildInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON CompilerInfo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CompilerInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON ComponentInfo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ComponentInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 10 . camelTo2 '-' }

