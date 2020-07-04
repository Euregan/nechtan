module Data.Type exposing (Type(..), toString)


type Type
  = INT
  | VARCHAR Int
  | CHAR Int
  | DATE
  | TINYINT
  | FLOAT

toString : Type -> String
toString kind =
  case kind of
    INT -> "INT"
    VARCHAR length -> "VARCHAR(" ++ String.fromInt length ++ ")"
    CHAR length -> "CHAR(" ++ String.fromInt length ++ ")"
    DATE -> "DATE"
    TINYINT -> "TINYINT"
    FLOAT -> "FLOAT"
