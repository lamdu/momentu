{-# LANGUAGE TemplateHaskell, DerivingVia #-}

module GUI.Momentu.I18N where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Element.Id (ElemIds)

import           GUI.Momentu.Prelude

data Texts a = Texts
    { _edit :: a
    , _view :: a
    , _insert :: a
    , _delete :: a
    , _navigation :: a
    , _move :: a
    , _choose :: a
    , _forward :: a
    , _backward :: a
    , _language :: a
    } deriving stock (Eq, Generic, Generic1, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Texts)
    deriving anyclass ElemIds
Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts

englishTexts :: Texts Text
englishTexts = Texts
    { _language = "Language"
    , _edit = "Edit"
    , _view = "View"
    , _insert = "Insert"
    , _delete = "Delete"
    , _move = "Move"
    , _navigation = "Navigation"
    , _choose = "Choose"
    , _forward = "Forward"
    , _backward = "Backward"
    }
