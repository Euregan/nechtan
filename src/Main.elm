port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, ul, li, textarea, text)
import Html.Attributes exposing (value, id)
import Html.Events exposing (onInput)
import Data.Size exposing (Size)
import Data.Table exposing (Table)
import Data.Database exposing (Database)
import Parser exposing (DeadEnd, Problem(..), run)
import SqlParser exposing (createDatabase)
import Visualization.Database as Database


import Data.Type exposing (Type(..))
import Data.Modifier exposing (Modifier(..))
import Data.Field exposing (Field)
import Data.Table exposing (Table)
userTable : Table
userTable =
  Table
    "user"
    [ Field "id" INT [PrimaryKey, NotNullable]
    , Field "firstName" (VARCHAR 100) []
    , Field "lastName" (VARCHAR 100) []
    , Field "email" (VARCHAR 255) []
    , Field "address1" (VARCHAR 255) []
    , Field "address2" (VARCHAR 255) []
    , Field "address3" (VARCHAR 255) []
    , Field "country" (VARCHAR 255) []
    , Field "city" (VARCHAR 255) []
    , Field "zipCode" (VARCHAR 5) []
    ]
orderTable : Table
orderTable =
  Table
    "order"
    [ Field "id" INT [PrimaryKey, NotNullable]
    , Field "user" INT [ForeignKey "user" "id"]
    ]
orderProductTable : Table
orderProductTable =
  Table
    "order_product"
    [ Field "order" INT [ForeignKey "order" "id"]
    , Field "product" INT [ForeignKey "product" "id"]
    ]
productTable : Table
productTable =
  Table
    "product"
    [ Field "id" INT [PrimaryKey, NotNullable]
    , Field "name" (VARCHAR 100) [NotNullable]
    , Field "price" FLOAT [NotNullable]
    ]


main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> (Model (Database "marketplace" [orderProductTable, productTable, orderTable, userTable]) [] (Size 0 0), Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Model =
  { database: Database
  , errors: List DeadEnd
  , schemaSize: Size
  }

type Msg
  = UpdatedQuery String
  | ResizeHappened Size

update msg model =
  case msg of
    UpdatedQuery query ->
      case run createDatabase query of
        Ok result ->
          ( { model | errors = [], database = result }
          , Cmd.none
          )
        Err errors ->
          ( { model | errors = errors }
          , Cmd.none
          )
    ResizeHappened size ->
      ( { model | schemaSize = size }
      , Cmd.none
      )

view model =
  div [ id "dashboard" ]
    [ div
      [ id "schema-container"
      ]
      [ Database.graph model.schemaSize model.database ]
    , div []
      [ textarea [ onInput UpdatedQuery ] []
      , ul [] <| List.map (\deadEnd -> li [] [ text <| problemToString deadEnd.problem ]) model.errors
      ]
    ]

problemToString : Problem -> String
problemToString problem =
  case problem of
    Expecting expected -> "Expected " ++ expected
    ExpectingInt -> "Expected an integer"
    ExpectingHex -> "Expected an hex"
    ExpectingOctal -> "Expected an octal"
    ExpectingBinary -> "Expected a binary"
    ExpectingFloat -> "Expected a float"
    ExpectingNumber -> "Expected a number"
    ExpectingVariable -> "Expected a variable"
    ExpectingSymbol symbol -> "Expected symbol " ++ symbol
    ExpectingKeyword keyword -> "Expected keyword " ++ keyword
    ExpectingEnd -> "Expected the end"
    UnexpectedChar -> "Unexpected character"
    Problem prob -> "Problem: " ++ prob
    BadRepeat -> "Bad repetition"

port resizeHappened : (Size -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  resizeHappened ResizeHappened
