{-# LANGUAGE TemplateHaskell #-}

module Language.SQL.TH (
    sql
) where

import           Control.Applicative
import           Control.Monad.Trans

import           Data.Char
import           Data.List
import           Data.String
import           Text.Parsec                 hiding (many, (<|>))

import           Language.Haskell.Meta.Parse
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax  as TH

import           Language.SQL.Builder

type Parser = ParsecT String () Q

-- | Generate a 'B.ByteString' expression using the given 'String'.
liftString :: String -> Q Exp
liftString str =
    AppE (VarE 'fromString) <$> TH.lift str

-- | Generate a 'Code' expression.
toCodeExp :: String -> Parser Exp
toCodeExp strCode =
    lift (liftString strCode)

-- | Something in parentheses
inParentheses :: Parser String
inParentheses = do
    char '('
    body <- many $ choice [ inParentheses
                          , quotation '"'
                          , quotation '\''
                          , some (noneOf "'\"()") ]
    ('(' : concat body ++ ")") <$ char ')'

-- | Quotation
quotation :: Char -> Parser String
quotation delim = do
    char delim
    body <- many $
        liftA2 (\ a b -> [a, b]) (char '\\') anyChar
        <|> (pure <$> noneOf [delim])
    (delim : concat body ++ [delim]) <$ char delim

-- | Name pattern
valueName :: Parser Name
valueName = do
    strName <- liftA2 (:) (letter <|> char '_') (many (alphaNum <|> char '_'))
    lift $ do
        mbName <- lookupValueName strName
        case mbName of
            Nothing      -> fail ("Name " ++ show strName ++ " does not refer to a value")
            Just valName -> pure valName

-- | Arrow code
arrowCode :: Parser Exp
arrowCode = do
    char '#'
    body <- inParentheses
    case parseExp body of
        Left msg  -> lift (fail msg)
        Right exp -> pure (AppE (VarE 'hole) exp)

-- | Arrow name
arrowName :: Parser Exp
arrowName = do
    char '#'
    AppE (VarE 'hole) . VarE <$> valueName

-- | Inline code
inlineCode :: Parser Exp
inlineCode = do
    char '$'
    body <- inParentheses
    case parseExp body of
        Left msg  -> lift (fail msg)
        Right exp -> pure exp

-- | Inline name
inlineName :: Parser Exp
inlineName = do
    char '$'
    VarE <$> valueName

-- | Parser for SQL code
sqlCode :: Parser Exp
sqlCode =
    fmap (AppE (VarE 'mconcat) . ListE) $ many $
        choice [ quotation '"' >>= toCodeExp
               , quotation '\'' >>= toCodeExp
               , try arrowCode
               , arrowName
               , try inlineCode
               , inlineName
               , some (noneOf "\"'#$") >>= toCodeExp ]

-- | Parse SQL code and generate a 'SQL' expression from it.
parseSqlCode :: String -> Q Exp
parseSqlCode inputCode = do
    result <-
        runParserT
            (sqlCode <* eof)
            ()
            "quasi-quote"
            (dropWhileEnd isSpace (dropWhile isSpace inputCode))
    case result of
        Left parseError -> do
            Loc _ _ _ (lineStart, charStart) _ <- location
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

        Right result ->
            pure result

-- | Basic SQL quasi-quoter
sql :: QuasiQuoter
sql =
    QuasiQuoter { quoteExp  = parseSqlCode
                , quoteDec  = const (error "'sql' quasi-quoter works only in expressions")
                , quotePat  = const (error "'sql' quasi-quoter works only in expressions")
                , quoteType = const (error "'sql' quasi-quoter works only in expressions") }
