module Language.SQL.Builder (
    Builder,
    hole,
    code,
    flatten
) where

import           Prelude               hiding (foldr, id, (.))

import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Data.Profunctor

import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Char8 as CharString
import           Data.Foldable
import qualified Data.Sequence         as Seq
import           Data.String

-- | Builder element
data Element input output
    = Hole (input -> output)
    | Code ByteString.ByteString

instance Functor (Element input) where
    {-# INLINE fmap #-}

    fmap mapOutput (Hole gen)  = Hole (mapOutput . gen)
    fmap _         (Code code) = Code code

instance Profunctor Element where
    {-# INLINE dimap #-}

    dimap mapInput mapOutput (Hole gen)  = Hole (mapOutput . gen . mapInput)
    dimap _        _         (Code code) = Code code

    {-# INLINE lmap #-}

    lmap mapInput (Hole gen)  = Hole (gen . mapInput)
    lmap _        (Code code) = Code code

    {-# INLINE rmap #-}

    rmap = fmap

{-# INLINE mapHole #-}

-- | Transform the 'Hole', if it is one.
mapHole
    :: ((input -> output) -> (input' -> output'))
    -> Element input output
    -> Element input' output'
mapHole trans (Hole gen)  = Hole (trans gen)
mapHole _     (Code code) = Code code

-- | Builder
newtype Builder input output = Builder { unBuilder :: Seq.Seq (Element input output) }

instance Show (Builder input output) where
    show builder =
        CharString.unpack (fst (flatten (\ index _ -> fromString ('$' : show (index + 1))) builder))

instance Monoid (Builder input output) where
    {-# INLINE mempty #-}

    mempty = Builder Seq.empty

    {-# INLINE mappend #-}

    mappend (Builder lhs) (Builder rhs) = Builder (lhs Seq.>< rhs)

    {-# INLINE mconcat #-}

    mconcat builders = Builder (mconcat (unBuilder <$> builders))

instance Functor (Builder input) where
    {-# INLINE fmap #-}

    fmap mapOutput (Builder elements) = Builder (fmap mapOutput <$> elements)

instance Profunctor Builder where
    {-# INLINE dimap #-}

    dimap mapInput mapOutput (Builder elements) = Builder (dimap mapInput mapOutput <$> elements)

    {-# INLINE lmap #-}

    lmap mapInput (Builder elements) = Builder (lmap mapInput <$> elements)

    {-# INLINE rmap #-}

    rmap mapOutput (Builder elements) = Builder (rmap mapOutput <$> elements)

instance Category Builder where
    {-# INLINE id #-}

    id = Builder (Seq.singleton (Hole id))

    Builder lhs . Builder rhs =
        Builder (join (fillHole <$> rhs))
        where
            fillHole (Hole gen)  = lmap gen <$> lhs
            fillHole (Code code) = Seq.singleton (Code code)

instance Arrow Builder where
    {-# INLINE arr #-}

    arr = hole

    {-# INLINE first #-}

    first = mapHoles first

    {-# INLINE second #-}

    second = mapHoles second

instance IsString (Builder input output) where
    fromString str = code (fromString str)

-- | Map all the 'Hole's inside the 'Builder'.
mapHoles
    :: ((input -> output) -> (input' -> output'))
    -> Builder input output
    -> Builder input' output'
mapHoles trans (Builder lhs) = Builder (mapHole trans <$> lhs)

{-# INLINE hole #-}

-- | Produce a 'Builder' that only consists of a 'Hole'.
hole :: (input -> output) -> Builder input output
hole gen = Builder (Seq.singleton (Hole gen))

{-# INLINE code #-}

-- | Produce a 'Builder' that only consists of 'Code'.
code :: ByteString.ByteString -> Builder input output
code code = Builder (Seq.singleton (Code code))

-- | Flatten 'Builder'.
flatten
    :: (Word -> (input -> output) -> ByteString.ByteString)
    -> Builder input output
    -> (ByteString.ByteString, [input -> output])
flatten holeCode (Builder elements) =
    (code, gens)
    where
        (_, code, gens) = foldr f (0, ByteString.empty, []) elements

        f elem (index, codes, gens) =
            case elem of
                Hole gen ->
                    ( index + 1
                    , ByteString.append (holeCode index gen) codes
                    , gen : gens )

                Code code ->
                    ( index
                    , ByteString.append code codes
                    , gens )
