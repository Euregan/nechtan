module SqlParser exposing (..)

import Parser exposing (..)
import Set
import Data.Type exposing (Type(..))
import Data.Modifier exposing (Modifier(..))
import Data.Field exposing (Field)
import Data.Table exposing (Table)


maybe : Parser a -> Parser (Maybe a)
maybe parser =
  oneOf
    [ map (\raw -> Just raw) parser
    , succeed Nothing
    ]

list : Parser a -> Parser (List a)
list parser =
  sequence
    { start = "("
    , separator = ","
    , end = ")"
    , spaces = spaces
    , item = parser
    , trailing = Forbidden
    }


identifier : Parser String
identifier =
  succeed (\x -> x)
    |. oneOf [symbol "`", succeed ()]
    |= variable
      { start = Char.isAlphaNum
      , inner = \c -> Char.isAlphaNum c || c == '_'
      , reserved = Set.fromList []
      }
    |. oneOf [symbol "`", succeed ()]


kind : Parser Type
kind =
  oneOf
    [ map (\_ -> TINYINT) <| symbol "TINYINT"
    , map (\_ -> INT) <| symbol "INT"
    , map (\_ -> DATE) <| symbol "DATE"
    , map VARCHAR <| succeed (\x -> x)
      |. symbol "VARCHAR("
      |= int
      |. symbol ")"
    , map CHAR <| succeed (\x -> x)
      |. symbol "CHAR("
      |= int
      |. symbol ")"
    ]

modifier : Parser Modifier
modifier =
  oneOf
    [ map (\_ -> PrimaryKey) <| symbol "PRIMARY KEY"
    , map (\_ -> NotNullable) <| symbol "NOT NULL"
    ]

field : Parser Field
field =
  succeed Field
    |= identifier
    |. spaces
    |= kind
    |. spaces
    |= sequence
      { start = ""
      , separator = " "
      , end = ""
      , spaces = symbol ""
      , item = modifier
      , trailing = Forbidden
      }

createTable : Parser Table
createTable =
  succeed Table
    |. keyword "CREATE TABLE"
    |. spaces
    |= identifier
    |. spaces
    |= list field
    |. spaces
    |. symbol ";"
    |. end
