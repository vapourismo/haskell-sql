{-# LANGUAGE TemplateHaskell #-}

module Language.SQL.TH (
    sql
) where

import           Control.Applicative
import           Control.Monad.Trans

import qualified Codec.Binary.UTF8.String   as UTF8
import qualified Data.ByteString            as B
import           Data.List
import           Text.Parsec                hiding (many, (<|>))

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH

import           Language.SQL.Types

type Parser = ParsecT String () Q

-- | Generate a 'B.ByteString' expression using the given 'String'.
liftAsByteString :: String -> Q Exp
liftAsByteString str =
    [e| B.pack $(TH.lift (UTF8.encode str)) |]

-- | Generate a 'Code' expression.
toCodeExp :: String -> Parser Exp
toCodeExp code =
    lift [e| Code $(liftAsByteString code) |]

-- | Parameter
param :: Parser Exp
param = do
    char '?'
    lift [e| appendSql dynamicParam |]

-- | Quote in SQL code
quote :: Char -> Parser Exp
quote delim = do
    char delim
    body <- many (escaped <|> pure <$> noneOf [delim])
    char delim

    let code = delim : concat body ++ [delim]
    toCodeExp code

    where
        escaped = (\ a b -> [a, b]) <$> char '\\' <*> anyChar

-- | Static parameter
static :: Parser Exp
static = do
    char '$'

    nameHead <- letter <|> char '_'
    nameBody <- many (alphaNum <|> char '_')
    let strName = nameHead : nameBody

    lift $ do
        mbName <- lookupValueName strName
        case mbName of
            Nothing      -> fail ("Name " ++ show strName ++ " does not refer to a value")
            Just valName -> [e| appendSql (staticParam $(varE valName)) |]

-- | Parser for SQL code
sqlCode :: Parser Exp
sqlCode = do
    segments <- many $ choice [ param
                              , static
                              , quote '"'
                              , quote '\''
                              , toCodeExp =<< some (noneOf "\"'?$") ]
    lift $ do
        nil <- [e| Nil |]
        pure (foldr AppE nil segments)

-- | Parse SQL code and generate a 'SQL' expression from it.
parseSqlCode :: String -> Q Exp
parseSqlCode code = do
    Loc _ _ _ (lineStart, charStart) _ <- location
    let reportParseError parseError = do
            let sourcePos = errorPos parseError
                lineReal  = lineStart + sourceLine sourcePos - 1
                charReal  = charStart + sourceColumn sourcePos - 1

            fail $
                "(line " ++ show lineReal ++ ", column " ++ show charReal ++ "): "
                ++ intercalate ", " (tail (lines (show parseError)))

    runParserT sqlCode () "quasi-quote" code >>= either reportParseError pure

-- | Basic SQL quasi-quoter
sql :: QuasiQuoter
sql =
    QuasiQuoter { quoteExp  = parseSqlCode
                , quoteDec  = const (error "'sql' quasi-quoter works only in expressions")
                , quotePat  = const (error "'sql' quasi-quoter works only in expressions")
                , quoteType = const (error "'sql' quasi-quoter works only in expressions") }
