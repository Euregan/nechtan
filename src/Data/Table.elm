module Data.Table exposing (Table)

import Data.Field exposing (Field)


type alias Table =
  { name: String
  , fields: List Field
  }
