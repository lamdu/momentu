module GUI.Momentu.DataFiles
    ( getDefaultFontPath
    , getDataFileName
    ) where

import Paths_momentu (getDataFileName)

import GUI.Momentu.Prelude

getDefaultFontPath :: IO FilePath
getDefaultFontPath = getDataFileName "fonts/DejaVuSans.ttf"
