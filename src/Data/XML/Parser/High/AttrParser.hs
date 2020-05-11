{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- | All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
-- > import Data.XML.Parser.High
module Data.XML.Parser.High.AttrParser
  ( AttrParser(..)
  , anyAttr
  , noAttr
  , attrValue
  , hasAttr
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Compat
import           Control.Monad.Fail.Compat
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.XML.Parser.Low.Name
import           Prelude.Compat

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString
-- >>> import Data.XML.Parser.High

-- | How to parse tag attributes.
newtype AttrParser a = AttrParser { runAttrParser :: Map QName Text -> Either String a }

deriving instance Functor AttrParser
deriving via (WrappedArrow (Kleisli (Either String)) (Map QName Text)) instance Applicative AttrParser

-- | Can be combined with @\<|\>@
deriving via (WrappedArrow (Kleisli (Either String)) (Map QName Text)) instance Alternative AttrParser

-- | Can be combined with @>>=@. Attributes map is forwarded without change.
instance Monad AttrParser where
  (AttrParser f) >>= g = AttrParser $ \attributes -> do
    a <- f attributes
    let AttrParser g' = g a
    g' attributes

instance MonadFail AttrParser where
  fail message = AttrParser $ const $ Left message

-- | Parse any set of attributes.
--
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr noContent) "<tag></tag>"
-- Right ()
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr noContent) "<tag key='value'></tag>"
-- Right ()
anyAttr :: AttrParser ()
anyAttr = pure ()

-- | Assert that no attributes exist.
--
-- >>> parseOnly (runTokenParser $ tag' anyName noAttr noContent) "<tag></tag>"
-- Right ()
-- >>> parseOnly (runTokenParser $ tag' anyName noAttr noContent) "<tag key='value'></tag>"
-- Left ...
noAttr :: AttrParser ()
noAttr = AttrParser $ \attributes -> if null attributes then Right () else Left $ "Expected no attribute, instead got: " <> show attributes

-- | Parse attribute by name, and return its value.
--
-- >>> parseOnly (runTokenParser $ tag' anyName (attrValue "foo") noContent) "<tag></tag>"
-- Left ...
-- >>> parseOnly (runTokenParser $ tag' anyName (attrValue "foo") noContent) "<tag foo='bar'></tag>"
-- Right ()
attrValue :: QName -> AttrParser Text
attrValue name = AttrParser $ maybe (Left $ "Missing attribute named " <> show name) Right . Map.lookup name

-- | Assert that an attribute exists, with given name and value.
--
-- >>> parseOnly (runTokenParser $ tag' anyName (hasAttr "foo" "bar") noContent) "<tag></tag>"
-- Left ...
-- >>> parseOnly (runTokenParser $ tag' anyName (hasAttr "foo" "bar") noContent) "<tag foo='baz'></tag>"
-- Left ...
-- >>> parseOnly (runTokenParser $ tag' anyName (hasAttr "foo" "bar") noContent) "<tag foo='bar'></tag>"
-- Right ()
hasAttr :: QName -> Text -> AttrParser ()
hasAttr name value = attrValue name >>= \value' -> if value == value' then pure () else fail $ "Expected attribute value " <> Text.unpack value <> ", instead got: " <> Text.unpack value'
