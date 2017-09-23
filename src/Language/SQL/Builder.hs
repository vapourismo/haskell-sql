{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Language.SQL.Builder (
    Builder (..),
    buildCode,
    buildParams,

    nest,
    unnest,
    isolateParams,
    type (++),
    append,
    Function,
    withParams
) where

import           Control.Arrow
import           Control.Monad.RWS.Strict

import qualified Data.ByteString          as B
import qualified Data.ByteString.UTF8     as UTF8
import           Data.String

-- | Query builder
data Builder ts p where
    Code  :: B.ByteString -> Builder ts p        -> Builder ts       p
    Param :: p            -> Builder ts p        -> Builder ts       p
    Nest  ::                 Builder ts (t -> p) -> Builder (t : ts) p
    Nil   ::                                        Builder '[]      p

instance Functor (Builder ts) where
    fmap f segment =
        case segment of
            Code  code  rest -> Code  code      (fmap f rest)
            Param param rest -> Param (f param) (fmap f rest)
            Nest  inner      -> Nest (fmap (fmap f) inner)
            Nil              -> Nil

instance Applicative (Builder '[]) where
    pure x = Param x Nil

    Code  code rest <*> rhs = Code code (rest <*> rhs)
    Param f    rest <*> rhs = append (f <$> rhs) (rest <*> rhs)
    Nil             <*> _   = Nil

instance Applicative (Builder ts) => Applicative (Builder (t : ts)) where
    pure x = Nest (const <$> pure x)

    lhs <*> rhs = Nest ((<*>) <$> unnest lhs <*> unnest rhs)

data DerivedStatic = DerivedStatic deriving Show

instance (Show p) => Show (Builder ts p) where
    show builder =
        concat (showBuilder builder)
        where
            showBuilder :: Show p => Builder ts p -> [String]
            showBuilder segment =
                case segment of
                    Code  code  rest -> UTF8.toString code                : showBuilder rest
                    Param param rest -> ("<param: " ++ show param ++ ">") : showBuilder rest
                    Nest  inner      -> showBuilder (DerivedStatic <$ inner)
                    Nil              -> []

instance IsString (Builder '[] p) where
    fromString str = Code (UTF8.fromString str) Nil

instance Monoid (Builder '[] p) where
    mempty = Code B.empty Nil

    mappend = append

-- | Alias for 'Nest'.
nest :: Builder ts (t -> p) -> Builder (t : ts) p
nest = Nest

-- | Dual of 'nest'.
unnest :: Builder (t : ts) p -> Builder ts (t -> p)
unnest segment =
    case segment of
        Code  code  rest -> Code  code          (unnest rest)
        Param param rest -> Param (const param) (unnest rest)
        Nest  inner      -> inner

-- | Remove code segments from a 'Builder'.
isolateParams :: Builder ts p -> Builder ts p
isolateParams segment =
    case segment of
        Code  _     rest -> isolateParams rest
        Param param rest -> Param param (isolateParams rest)
        Nest  inner      -> Nest (isolateParams inner)
        Nil              -> Nil

-- | Append two type lists.
type family (++) ts us where
    (++) '[]      us  = us
    (++) (t : ts) us  = t : ts ++ us

-- | Append 'two' builders.
append :: Builder ts p -> Builder us p -> Builder (ts ++ us) p
append segment rhs =
    case segment of
        Code  code  lhs -> Code  code  (append lhs rhs)
        Param param lhs -> Param param (append lhs rhs)
        Nil             -> rhs
        Nest  inner     -> Nest (append inner (const <$> rhs))

-- | @Function '[p1, p2, ... pn] r@ constructs a type signature @p1 -> p2 -> ... pn -> r@.
type family Function ts r where
    Function '[]      r = r
    Function (t : ts) r = t -> Function ts r

-- | Do something with parameters inside the builder.
withParams :: ([p] -> r) -> Builder ts p -> Function ts r
withParams ret segment =
    case segment of
        Code  _     rest  -> withParams ret               rest
        Param value rest  -> withParams (ret . (value :)) rest
        Nest        inner -> \ param -> withParams ret (($ param) <$> inner)
        Nil               -> ret []

-- | Reader-writer-state-transformer which gathers collects code segments.
buildCodeSegments :: Builder ts p -> RWS (Word -> B.ByteString) B.ByteString Word ()
buildCodeSegments segment =
    case segment of
        Code code rest -> do
            tell code
            buildCodeSegments rest

        Param _ rest -> do
            index <- state (id &&& (+ 1))
            asks ($ index) >>= tell
            buildCodeSegments rest

        Nest inner -> buildCodeSegments inner

        Nil -> pure ()

-- | @buildCode placeholderCode builder@ collects the code segments from a 'Builder'. It also
-- translates 'Param's to placeholders using the given @placeholderCode@. The parameter to
-- @placeholderCode@ is the parameter index. Indices start at 0.
buildCode :: (Word -> B.ByteString) -> Builder ts p -> B.ByteString
buildCode placeholder segment =
    snd (evalRWS (buildCodeSegments segment) placeholder 0)

-- | @buildParams sql@ produces a function which collects all the necessary parameters for the
-- query described in @sql@.
buildParams :: Builder ts p -> Function ts [p]
buildParams = withParams id
