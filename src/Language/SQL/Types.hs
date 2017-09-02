{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Language.SQL.Types (
    SQL (..),

    Query (..),
    PrepQuery (..),

    AppendSQL (..),
    Function,
    IsolateParams (..),
    WithValues (..)
) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String

-- | Basic SQL syntax tree
data SQL p ts where
    Code   :: B.ByteString -> SQL p ts -> SQL p ts
    Static :: p            -> SQL p ts -> SQL p ts
    Param  :: (t -> p)     -> SQL p ts -> SQL p (t : ts)
    Nil    ::                             SQL p '[]

instance IsString (SQL p ts -> SQL p ts) where
    fromString str = Code (UTF8.fromString str)

instance IsString (SQL p '[]) where
    fromString str = Code (UTF8.fromString str) Nil

instance Monoid (SQL p '[]) where
    mempty = Code B.empty Nil

    mappend = appendSql

type family (++) ts us where
    (++) '[]      us = us
    (++) (t : ts) us = t : ts ++ us

class AppendSQL ts where
    -- | Append two 'SQL' instances.
    appendSql :: SQL p ts -> SQL p us -> SQL p (ts ++ us)

instance AppendSQL '[] where
    appendSql segment rhs =
        case segment of
            Code   code  lhs -> Code   code  (appendSql lhs rhs)
            Static value lhs -> Static value (appendSql lhs rhs)
            Nil              -> rhs

instance AppendSQL ts => AppendSQL (t : ts) where
    appendSql segment rhs =
        case segment of
            Code   code    lhs -> Code   code    (appendSql lhs rhs)
            Static value   lhs -> Static value   (appendSql lhs rhs)
            Param  toValue lhs -> Param  toValue (appendSql lhs rhs)

class IsolateParams ts where
    -- | Isolate the parameters from the query.
    isolateParams :: SQL p ts -> SQL p ts

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

-- | @Function '[p1, p2, ... pn] r@ produces a function @p1 -> p2 -> ... pn -> r@.
type family Function ts r where
    Function '[]      r = r
    Function (t : ts) r = t -> Function ts r

class WithValues ts where
    -- | Do something with the values that need to be passed alongside a given query.
    withValues :: ([p] -> r) -> SQL p ts -> Function ts r

instance WithValues '[] where
    withValues ret segment =
        case segment of
            Static value rest -> withValues (ret . (value :)) rest
            Code   _     rest -> withValues ret               rest
            Nil               -> ret []

instance WithValues ts => WithValues (t : ts) where
    withValues ret segment param =
        case segment of
            Static value   rest -> withValues (ret . (value         :)) rest param
            Param  toValue rest -> withValues (ret . (toValue param :)) rest
            Code   _       rest -> withValues ret                       rest param

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
              , prepQueryParams :: SQL p ts }
