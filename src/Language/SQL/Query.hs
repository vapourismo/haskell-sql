{-# LANGUAGE MultiParamTypeClasses #-}

module Language.SQL.Query (
    dollarSign,
    questionMark
) where

import Data.String

import Language.SQL.Builder

-- | PostgreSQL-style placeholders for which indices start at 1
dollarSign :: IsString code => Word -> arr input output -> code
dollarSign index _ = fromString ('$' : show (index + 1))

-- | Question mark placeholder
questionMark :: IsString code => Word -> arr input output -> code
questionMark _ _ = fromString "?"
