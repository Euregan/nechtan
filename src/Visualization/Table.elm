module Visualization.Table exposing (box, boxSize, fieldY)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Data.Table exposing (Table)
import Data.Field exposing (Field)
import Data.Type as Type
import Data.Point exposing (Point)
import Data.Size exposing (Size)


padding = 15
margin = 15
fontHeight = 15
fontWidth = fontHeight * 0.60

textWidth : String -> Float
textWidth string =
  (toFloat (String.length string)) * fontWidth

boxSize : Table -> Size
boxSize table =
  let
    fieldsCount = toFloat (List.length table.fields)
    longestField = List.foldl (\longest current -> if String.length current > String.length longest then current else longest) "" <| List.map (\f -> f.name) table.fields
    longestType = List.foldl (\longest current -> if String.length current > String.length longest then current else longest) "" <| List.map (\f -> Type.toString f.kind) table.fields
    contentWidth = Basics.max ((textWidth longestField) + margin + (textWidth longestType)) (textWidth table.name)
  in
    Size
      (padding + contentWidth + padding)
      (padding + fontHeight + margin * (fieldsCount + 1) + fontHeight * fieldsCount + padding)

fieldY : Int -> Float
fieldY position =
  (padding + fontHeight + margin + (toFloat position) * margin + (toFloat position) * fontHeight + (fontHeight / 2))



box : Point -> Table -> Svg msg
box placement table =
  let
    longestField = List.foldl (\longest current -> if String.length current > String.length longest then current else longest) "" <| List.map (\f -> f.name) table.fields
  in
    g []
      [ box_ placement (boxSize table)
      , title_ (Point (placement.x + padding) (placement.y + padding + fontHeight)) table.name
      , g [] <| List.indexedMap (\index f -> field (Point (placement.x + padding) (placement.y + padding + fontHeight + margin * (toFloat index + 2) + fontHeight * (toFloat index + 1))) (textWidth longestField) f) table.fields
      ]

field : Point -> Float -> Field -> Svg msg
field placement typeOffset { name, kind } =
  g []
    [ text_
      [ x (String.fromFloat placement.x)
      , y (String.fromFloat placement.y)
      ]
      [ text name ]
    , text_
      [ x (String.fromFloat (placement.x + margin + typeOffset))
      , y (String.fromFloat placement.y)
      ]
      [ text <| Type.toString kind ]
    ]

title_ :  Point -> String -> Svg msg
title_ placement content =
  text_
   [ x (String.fromFloat placement.x)
   , y (String.fromFloat placement.y)
   , fontWeight "bold"
   ]
   [ text content ]

box_ : Point -> Size -> Svg msg
box_ placement size =
  rect
    [ x (String.fromFloat placement.x)
    , y (String.fromFloat placement.y)
    , width (String.fromFloat size.width)
    , height (String.fromFloat size.height)
    , fill "white"
    , rx "10"
    , Svg.Attributes.style "filter:url(#dropshadow)"
    ]
    []
