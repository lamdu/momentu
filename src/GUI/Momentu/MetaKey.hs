-- | Cross platform ModKey.

module GUI.Momentu.MetaKey
    ( OSString, cmdLens, cmd
    , parse, format
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           GUI.Momentu.ModKey (ModKey(..), ModifierKeys(..))
import qualified GUI.Momentu.ModKey as ModKey
import           Text.Read (readMaybe)

import           GUI.Momentu.Prelude

-- | The OS string as defined by the result of System.Info.os
type OSString = String

cmdLens :: OSString -> Lens.ALens' ModifierKeys Bool
cmdLens os
    | os == "darwin" = ModKey.mSuper
    | otherwise      = ModKey.mControl

cmd :: OSString -> ModKey.Key -> ModKey
cmd os = ModKey (mempty & cmdLens os #~ True)

parse :: OSString -> Text -> Either String ModKey
parse os s =
    case readMaybe ("Key'" ++ Text.unpack (last parts)) of
    Just k | numModsOn == length modsTexts ->
        Right ModKey
        { _key = k
        , _modifiers = mods
        }
    _ ->
        unwords ["Parsed", show mods, "mods from", show modsTexts]
        & Left
    where
        parts = Text.splitOn "+" s
        modsTexts = init parts
        cmdMods
            | "Cmd" `elem` modsTexts = mempty & cmdLens os #~ True
            | otherwise = mempty
        mods =
            cmdMods
            <> mempty
            { _mAlt = "Alt" `elem` modsTexts
            , _mShift = "Shift" `elem` modsTexts
            , _mSuper = "Super" `elem` modsTexts
            , _mControl = "Ctrl" `elem` modsTexts
            }
        numModsOn =
            f ModKey.mControl + f ModKey.mAlt + f ModKey.mShift + f ModKey.mSuper
            where
                f m
                    | mods ^. m = 1
                    | otherwise = 0

format :: OSString -> ModKey -> Text
format os (ModKey mods k) =
    ["Cmd+" | os == "darwin" && mods ^. ModKey.mSuper] ++
    ["Super+" | os /= "darwin" && mods ^. ModKey.mSuper] ++

    ["Cmd+" | os /= "darwin" && mods ^. ModKey.mControl] ++
    ["Control+" | os == "darwin" && mods ^. ModKey.mControl] ++

    ["Alt+" | mods ^. ModKey.mAlt] ++
    ["Shift+" | mods ^. ModKey.mShift] ++
    [show k & drop 4 & Text.pack]
    & mconcat
