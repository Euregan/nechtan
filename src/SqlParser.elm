module SqlParser exposing (..)

import Parser exposing (..)
import Set
import Data.Type exposing (Type(..))
import Data.Modifier exposing (Modifier(..))
import Data.Field exposing (Field)
import Data.Table exposing (Table)
import Data.Database exposing (Database)


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
    , map (\_ -> FLOAT) <| symbol "FLOAT"
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

foreignKey : Parser (String, Modifier)
foreignKey =
  succeed (\f foreignTable foreignField -> (f, ForeignKey foreignTable foreignField))
    |. keyword "FOREIGN KEY"
    |. spaces
    |. symbol "("
    |. spaces
    |= identifier
    |. spaces
    |. symbol ")"
    |. spaces
    |. keyword "REFERENCES"
    |. spaces
    |= identifier
    |. spaces
    |. symbol "("
    |. spaces
    |= identifier
    |. spaces
    |. symbol ")"

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

definitions : Parser (List Field)
definitions =
  succeed (\x -> x)
    |. symbol "("
    |. spaces
    |= loop [] (\fields -> oneOf
      [ backtrackable <| succeed (\f -> Loop (f :: fields))
        |. spaces
        |= field
      , backtrackable <| succeed (\f -> Loop (f :: fields))
        |. spaces
        |. symbol ","
        |. spaces
        |= field
      , backtrackable <| succeed (\(fieldName, fieldModifier) -> Loop (List.map (\f -> if f.name == fieldName then { f | modifiers = fieldModifier :: f.modifiers } else f) fields))
        |. spaces
        |. symbol ","
        |. spaces
        |= foreignKey
      , succeed ()
          |> map (\_ -> Done <| List.reverse fields)
      ] )
    |. spaces
    |. symbol ")"

createTable : Parser Table
createTable =
  succeed Table
    |. keyword "CREATE TABLE"
    |. spaces
    |= identifier
    |. spaces
    |= definitions

createDatabase : Parser Database
createDatabase =
  succeed Database
    |. keyword "CREATE DATABASE"
    |. spaces
    |= identifier
    |. spaces
    |. symbol ";"
    |. spaces
    |= loop [] (\tables -> oneOf
      [ backtrackable <| succeed (\table -> Loop (table :: tables))
        |. spaces
        |= createTable
        |. spaces
        |. symbol ";"
      , succeed ()
          |> map (\_ -> Done <| List.reverse tables)
      ] )
