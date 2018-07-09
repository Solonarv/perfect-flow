module Paths where

import           System.FilePath

dataDir :: FilePath
dataDir = "data"

assetDir :: FilePath
assetDir = dataDir </> "assets"

defaultFont :: FilePath
defaultFont = fontPath "OpenSans-Regular"

fontPath :: FilePath -> FilePath
fontPath font = assetDir </> "fonts" </> font <.> "ttf"

logPath :: FilePath
logPath = "logs" </> "log.txt"
