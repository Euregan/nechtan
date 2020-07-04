module Data.Modifier exposing (Modifier(..))


type Modifier
  = NotNullable
  | PrimaryKey
  | ForeignKey String String
