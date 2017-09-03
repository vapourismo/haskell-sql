{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.SQL.Builder (
    Builder (..),
    buildCode,
    buildCodeSegments,
    buildParams,

    type (++),
    Function,
    BuilderAux (..)
) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String

-- | Query builder
data Builder ts p where
    Code   :: B.ByteString         -> Builder ts p -> Builder ts       p
    Static :: p                    -> Builder ts p -> Builder ts       p
    Param  :: (t -> p)             -> Builder ts p -> Builder (t : ts) p
    Nil    ::                                         Builder '[]      p

-- | Append two type lists.
type family (++) ts us where
    (++) '[]      us = us
    (++) (t : ts) us = t : ts ++ us

-- | @Function '[p1, p2, ... pn] r@ constructs a type signature @p1 -> p2 -> ... pn -> r@.
type family Function ts r where
    Function '[]      r = r
    Function (t : ts) r = t -> Function ts r

-- | Auxiliary 'Builder'-related functions
class BuilderAux ts where
    -- | Show sub-components of the builder.
    showBuilder :: Show a => Builder ts a -> [String]

    -- | Map inner parameters.
    fmapBuilder :: (a -> b) -> Builder ts a -> Builder ts b

    -- | Append builders.
    appendBuilder :: Builder ts p -> Builder us p -> Builder (ts ++ us) p

    -- | Retain only parameters.
    isolateParams :: Builder ts p -> Builder ts p

    -- | Do something with parameters inside the builder.
    withParams :: ([p] -> r) -> Builder ts p -> Function ts r

instance BuilderAux '[] where
    showBuilder segment =
        case segment of
            Code   code  rest -> UTF8.toString code : showBuilder rest
            Static param rest -> ("<static: " ++ show param ++ ">") : showBuilder rest
            Nil               -> []

    fmapBuilder f segment =
        case segment of
            Code   code  rest -> Code   code      (fmapBuilder f rest)
            Static param rest -> Static (f param) (fmapBuilder f rest)
            Nil               -> Nil

    -- pureBuilder _ = Nil

    appendBuilder segment rhs =
        case segment of
            Code   code  lhs -> Code   code  (appendBuilder lhs rhs)
            Static value lhs -> Static value (appendBuilder lhs rhs)
            Nil              -> rhs

    isolateParams segment =
        case segment of
            Code   _     rest -> isolateParams rest
            Static value rest -> Static value (isolateParams rest)
            Nil               -> Nil

    withParams ret segment =
        case segment of
            Static value rest -> withParams (ret . (value :)) rest
            Code   _     rest -> withParams ret               rest
            Nil               -> ret []

instance BuilderAux ts => BuilderAux (t : ts) where
    showBuilder segment =
        case segment of
            Code   code   rest -> UTF8.toString code : showBuilder rest
            Static param  rest -> ("<static: " ++ show param ++ ">") : showBuilder rest
            Param  _      rest -> "<dynamic>" : showBuilder rest

    fmapBuilder f segment =
        case segment of
            Code   code    rest -> Code   code                          (fmapBuilder f rest)
            Static param   rest -> Static (f param)                     (fmapBuilder f rest)
            Param  toParam rest -> Param  (f . toParam)                 (fmapBuilder f rest)

    appendBuilder segment rhs =
        case segment of
            Code   code    lhs -> Code   code    (appendBuilder lhs rhs)
            Static value   lhs -> Static value   (appendBuilder lhs rhs)
            Param  toValue lhs -> Param  toValue (appendBuilder lhs rhs)

    isolateParams segment =
        case segment of
            Code   _       rest -> isolateParams rest
            Static value   rest -> Static value                  (isolateParams rest)
            Param  toValue rest -> Param  toValue                (isolateParams rest)

    withParams ret segment param =
        case segment of
            Code   _       rest -> withParams ret                                             rest param
            Static value   rest -> withParams (ret . (value         :))                       rest param
            Param  toValue rest -> withParams (ret . (toValue param :))                       rest

instance BuilderAux ts => Functor (Builder ts) where
    fmap = fmapBuilder

instance (BuilderAux ts, Show p) => Show (Builder ts p) where
    show builder = concat (showBuilder builder)

instance IsString (Builder ts p -> Builder ts p) where
    fromString str = Code (UTF8.fromString str)

instance IsString (Builder '[] p) where
    fromString str = Code (UTF8.fromString str) Nil

instance Monoid (Builder '[] p) where
    mempty = Code B.empty Nil

    mappend = appendBuilder

-- | Gather the code segments from the Builder syntax tree.
buildCodeSegments :: (Word -> B.ByteString) -> Word -> Builder ts p -> [B.ByteString]
buildCodeSegments placeholder index segment =
    case segment of
        Code   code rest -> code              : buildCodeSegments placeholder index       rest
        Static _    rest -> placeholder index : buildCodeSegments placeholder (index + 1) rest
        Param  _    rest -> placeholder index : buildCodeSegments placeholder (index + 1) rest
        Nil              -> []

-- | Gather the code from the Builder syntax tree.
buildCode :: (Word -> B.ByteString) -> Builder ts p -> B.ByteString
buildCode placeholder segments =
    B.concat (buildCodeSegments placeholder 0 segments)

-- | @buildParams sql@ produces a function which collects all the necessary parameters for the
-- query described in @sql@.
buildParams :: BuilderAux ts => Builder ts p -> Function ts [p]
buildParams =
    withParams id
