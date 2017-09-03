{-# LANGUAGE TemplateHaskell #-}

module Language.SQL.TH (
    sql
) where

import           Control.Applicative
import           Control.Monad.Trans

import qualified Codec.Binary.UTF8.String    as UTF8
import qualified Data.ByteString             as B
import           Data.Char
import           Data.List
import           Text.Parsec                 hiding (many, (<|>))

import           Language.Haskell.Meta.Parse
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax  as TH

import           Language.SQL.Builder
import           Language.SQL.Query

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
    lift [e| append (Nest dynamicParam) |]

-- | Quotation
quotation :: Char -> Parser String
quotation delim = do
    char delim
    body <- many (escaped <|> pure <$> noneOf [delim])
    char delim
    pure (delim : concat body ++ [delim])
    where
        escaped = (\ a b -> [a, b]) <$> char '\\' <*> anyChar

-- | Something in parentheses
inParentheses :: Parser String
inParentheses = do
    char '('
    body <- many $ choice [ inParentheses
                          , quotation '"'
                          , quotation '\''
                          , some (noneOf "'\"()") ]
    char ')'
    pure ('(' : concat body ++ ")")

-- | Inlined 'SQL'
inline :: Parser Exp
inline = do
    char '$'
    body <- inParentheses
    either fail (\ exp -> lift [e| append $(pure exp) |]) (parseExp body)

-- | Textual quote
quote :: Char -> Parser Exp
quote delim =
    quotation delim >>= toCodeExp

-- | Static parameter
static :: Parser Exp
static = do
    char '$'
    strName <- (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')

    lift $ do
        mbName <- lookupValueName strName
        case mbName of
            Nothing      -> fail ("Name " ++ show strName ++ " does not refer to a value")
            Just valName -> [e| append (staticParam $(varE valName)) |]

-- | Parser for SQL code
sqlCode :: Parser Exp
sqlCode =
    foldr AppE (ConE 'Nil)
    <$> many (choice [ param
                     , try inline
                     , static
                     , quote '"'
                     , quote '\''
                     , toCodeExp =<< some (noneOf "\"'?$") ])

-- | Parse SQL code and generate a 'SQL' expression from it.
parseSqlCode :: String -> Q Exp
parseSqlCode inputCode = do
    let code = dropWhileEnd isSpace (dropWhile isSpace inputCode)

    Loc _ _ _ (lineStart, charStart) _ <- location
    let reportParseError parseError = do
            let sourcePos = errorPos parseError
                lineReal  = lineStart + sourceLine sourcePos - 1
                charReal  = charStart + sourceColumn sourcePos - 1

            fail $
                "(line "
                ++ show lineReal
                ++ ", column "
                ++ show charReal
                ++ "): "
                ++ intercalate ", " (tail (lines (show parseError)))

    runParserT (sqlCode <* eof) () "quasi-quote" code >>= either reportParseError pure

-- | Basic SQL quasi-quoter
sql :: QuasiQuoter
sql =
    QuasiQuoter { quoteExp  = parseSqlCode
                , quoteDec  = const (error "'sql' quasi-quoter works only in expressions")
                , quotePat  = const (error "'sql' quasi-quoter works only in expressions")
                , quoteType = const (error "'sql' quasi-quoter works only in expressions") }
