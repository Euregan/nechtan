module Data.Database exposing (Database)

import Data.Table exposing (Table)


type alias Database =
  { name: String
  , tables: List Table
  }
