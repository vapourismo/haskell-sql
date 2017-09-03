{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.SQL.Builder (
    Placeholder (..),
    Param (..),

    buildCode,
    buildCodeSegments,
    buildValues,
    buildQuery,
    buildPrepQuery
) where

import qualified Data.ByteString    as B
import           Data.Tagged

import           Language.SQL.Types

-- | @p@ can be used as a placeholder.
class Placeholder p where
    -- | A function that produces placeholder code for a given index. Indices start at 0.
    placeholderCode :: Tagged p (Word -> B.ByteString)

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

-- | Gather the code segments from the Builder syntax tree.
buildCodeSegments :: (Word -> B.ByteString) -> Word -> Builder ts p -> [B.ByteString]
buildCodeSegments placeholder index segment =
    case segment of
        Code   code rest -> code              : buildCodeSegments placeholder index       rest
        Static _    rest -> placeholder index : buildCodeSegments placeholder (index + 1) rest
        Param  _    rest -> placeholder index : buildCodeSegments placeholder (index + 1) rest
        Nil              -> []

-- | Gather the code from the Builder syntax tree.
buildCode :: forall p ts. Placeholder p => Builder ts p -> B.ByteString
buildCode segments =
    B.concat (buildCodeSegments (untag (placeholderCode @p)) 0 segments)

-- | @buildValues sql@ produces a function which collects all the necessary parameters for the
-- query described in @sql@.
buildValues :: WithParams ts => Builder ts p -> Function ts [p]
buildValues =
    withParams id

-- | @buildQuery sql@ produces a function which collects all the parameters need by @sql@ in order
-- instantiate a 'Query'.
buildQuery :: (Placeholder p, WithParams ts) => Builder ts p -> Function ts (Query p)
buildQuery segment =
    withParams (Query (buildCode segment)) segment

-- | Build a 'PrepQuery' instance using the given query.
buildPrepQuery :: (Placeholder p, IsolateParams ts) => Builder ts p -> PrepQuery p ts
buildPrepQuery segment =
    PrepQuery (buildCode segment) (isolateParams segment)
