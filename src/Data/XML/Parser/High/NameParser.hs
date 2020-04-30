{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- | All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
-- > import Data.XML.Parser.High
module Data.XML.Parser.High.NameParser
  ( NameParser(..)
  , anyName
  , anyNameExcept
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.String
import           Data.XML.Parser.Low

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString
-- >>> import Data.XML.Parser.High

-- | How to parse tag names.
newtype NameParser a = NameParser { runNameParser :: QName -> Maybe a }

deriving instance Functor NameParser
deriving via (WrappedArrow (Kleisli Maybe) QName) instance Applicative NameParser

-- | Can be combined with @\<|\>@
deriving via (WrappedArrow (Kleisli Maybe) QName) instance Alternative NameParser

-- | Match a single 'QName' in a concise way.
--
-- >>> parseOnly (runTokenParser $ tag' "foo" anyAttr anyContent) "<foo></foo>"
-- Right ()
instance (a ~ ()) => IsString (NameParser a) where
  fromString s = NameParser $ \(QName _ name) ->
    unless (fromString s == name) mempty

-- | Match any qualified name.
anyName :: NameParser QName
anyName = NameParser Just

-- | Match any qualified name, except for the given value.
--
-- >>> parseOnly (runTokenParser $ tag' (anyNameExcept "foo") anyAttr anyContent) "<foo></foo>"
-- Left ...
-- >>> parseOnly (runTokenParser $ tag' (anyNameExcept "foo") anyAttr anyContent) "<bar></bar>"
-- Right ()
anyNameExcept :: QName -> NameParser QName
anyNameExcept name = NameParser $ \name' -> do
  guard $ name /= name'
  return name'
