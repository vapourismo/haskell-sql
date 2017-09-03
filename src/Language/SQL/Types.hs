{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Language.SQL.Types (
    Builder (..),

    Query (..),
    PrepQuery (..),

    AppendBuilder (..),
    Function,
    IsolateParams (..),
    WithParams (..)
) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String

-- | Query builder
data Builder ts p where
    Code   :: B.ByteString -> Builder ts p -> Builder ts       p
    Static :: p            -> Builder ts p -> Builder ts       p
    Param  :: (t -> p)     -> Builder ts p -> Builder (t : ts) p
    Nil    ::                                 Builder '[]      p

instance IsString (Builder ts p -> Builder ts p) where
    fromString str = Code (UTF8.fromString str)

instance IsString (Builder '[] p) where
    fromString str = Code (UTF8.fromString str) Nil

instance Monoid (Builder '[] p) where
    mempty = Code B.empty Nil

    mappend = appendBuilder

type family (++) ts us where
    (++) '[]      us = us
    (++) (t : ts) us = t : ts ++ us

class AppendBuilder ts where
    -- | Append two 'Builder' instances.
    appendBuilder :: Builder ts p -> Builder us p -> Builder (ts ++ us) p

instance AppendBuilder '[] where
    appendBuilder segment rhs =
        case segment of
            Code   code  lhs -> Code   code  (appendBuilder lhs rhs)
            Static value lhs -> Static value (appendBuilder lhs rhs)
            Nil              -> rhs

instance AppendBuilder ts => AppendBuilder (t : ts) where
    appendBuilder segment rhs =
        case segment of
            Code   code    lhs -> Code   code    (appendBuilder lhs rhs)
            Static value   lhs -> Static value   (appendBuilder lhs rhs)
            Param  toValue lhs -> Param  toValue (appendBuilder lhs rhs)

class IsolateParams ts where
    -- | Isolate the parameters from the query.
    isolateParams :: Builder ts p -> Builder ts p

instance IsolateParams '[] where
    isolateParams segment =
        case segment of
            Code   _     rest -> isolateParams rest
            Static value rest -> Static value (isolateParams rest)
            Nil               -> Nil

instance IsolateParams (t : ts) where
    isolateParams segment =
        case segment of
            Code   _       rest -> isolateParams rest
            Static value   rest -> Static value   rest
            Param  toValue rest -> Param  toValue rest

-- | @Function '[p1, p2, ... pn] r@ constructs a type signature @p1 -> p2 -> ... pn -> r@.
type family Function ts r where
    Function '[]      r = r
    Function (t : ts) r = t -> Function ts r

class WithParams ts where
    -- | Do something with the values that need to be passed alongside a given query.
    withParams :: ([p] -> r) -> Builder ts p -> Function ts r

instance WithParams '[] where
    withParams ret segment =
        case segment of
            Static value rest -> withParams (ret . (value :)) rest
            Code   _     rest -> withParams ret               rest
            Nil               -> ret []

instance WithParams ts => WithParams (t : ts) where
    withParams ret segment param =
        case segment of
            Static value   rest -> withParams (ret . (value         :)) rest param
            Param  toValue rest -> withParams (ret . (toValue param :)) rest
            Code   _       rest -> withParams ret                       rest param

-- | Query
data Query p =
    Query { queryCode   :: B.ByteString
          , queryParams :: [p] }

deriving instance (Show p) => Show (Query p)
deriving instance (Eq p)   => Eq (Query p)
deriving instance (Ord p)  => Ord (Query p)

-- | Preparable query
data PrepQuery p ts =
    PrepQuery { prepQueryCode   :: B.ByteString
              , prepQueryParams :: Builder ts p }
