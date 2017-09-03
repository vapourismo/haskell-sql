{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Language.SQL.Types (
    Query (..),
    PrepQuery (..),

    Param (..)
) where

import qualified Data.ByteString      as B

import           Language.SQL.Builder

-- | Query
data Query p =
    Query { queryCode   :: B.ByteString
          , queryParams :: [p] }

deriving instance (Show p) => Show (Query p)
deriving instance (Eq p)   => Eq (Query p)
deriving instance (Ord p)  => Ord (Query p)

-- | Preparable query
data PrepQuery ts p =
    PrepQuery { prepQueryCode   :: B.ByteString
              , prepQueryParams :: Builder ts p }

-- | @a@ can be used as parameter type @p@.
class Param p a where
    -- | Generate 'Builder' for a static parameter.
    staticParam :: a -> Builder '[] p
    staticParam a = Static (toValue a) Nil

    -- | Generate 'Builder' for a dynamic parameter.
    dynamicParam :: Builder '[a] p
    dynamicParam = Param toValue Nil

    -- | Convert to value.
    toValue :: a -> p
