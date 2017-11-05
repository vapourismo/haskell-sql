module Language.SQL.Query (
    postgresPlaceholder,
    mysqlPlaceholder,
    mariadbPlaceholder
) where

import Data.String

import Language.SQL.Builder

postgresPlaceholder :: IsString code => Word -> arr input output -> code
postgresPlaceholder index _ =
    fromString ('$' : show (index + 1))

mysqlPlaceholder :: IsString code => Word -> arr input output -> code
mysqlPlaceholder _ _ =
    fromString "?"

mariadbPlaceholder :: IsString code => Word -> arr input output -> code
mariadbPlaceholder =
    mysqlPlaceholder
