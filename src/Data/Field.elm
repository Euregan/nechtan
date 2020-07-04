module Data.Field exposing (Field)

import Data.Type exposing (Type)
import Data.Modifier exposing (Modifier)


type alias Field =
  { name: String
  , kind: Type
  , modifiers: List Modifier
  }
