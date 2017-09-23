{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Language.SQL.Query (
    Query (..),
    buildQuery,

    PrepQuery (..),
    buildPrepQuery,

    Placeholder (..),
    Param (..)
) where

import qualified Data.ByteString      as B
import           Data.Hashable
import           Data.Semigroup
import           Data.String
import           Data.Tagged

import           Language.SQL.Builder

-- | Query
data Query ts p =
    Query { queryCode   :: B.ByteString
          , queryParams :: Builder ts p }
    deriving Show

class Placeholder p where
    -- | Generate the placeholder code for a parameter at a given index.
    placeholderCode :: Tagged p (Word -> B.ByteString)

instance Placeholder p => Placeholder (a -> p) where
    placeholderCode =
        Tagged (untag @p placeholderCode)

-- | Build a 'Query' from a 'Builder'.
buildQuery :: forall ts p. Placeholder p => Builder ts p -> Query ts p
buildQuery builder =
    Query (buildCode (untag @p placeholderCode) builder)
          (isolateParams builder)

-- | Preparable query
data PrepQuery ts p =
    PrepQuery { prepName     :: B.ByteString
              , prepCodeHash :: Int
              , prepQuery    :: Query ts p }
    deriving Show

-- | Build a 'PrepQuery' from a 'Builder'.
buildPrepQuery :: Placeholder p => Builder ts p -> PrepQuery ts p
buildPrepQuery builder =
    PrepQuery name codeHash query
    where
        query    = buildQuery builder
        codeHash = hash (queryCode query)
        name     = "PrepQuery" <> nameHashComp

        nameHashComp
            | codeHash < 0 = fromString ("0" ++ show (abs codeHash))
            | otherwise    = fromString (show codeHash)

-- | @a@ can be used as parameter type @p@.
class Param p a where
    -- | Generate 'Builder' for a static parameter.
    staticParam :: a -> Builder '[] p
    staticParam a = ($ a) <$> dynamicParam

    -- | Generate 'Builder' for a dynamic parameter.
    dynamicParam :: Builder '[] (a -> p)
    dynamicParam = Param toValue Nil

    -- | Convert to value.
    toValue :: a -> p

instance {-# OVERLAPPABLE #-} Param p p where
    toValue = id
