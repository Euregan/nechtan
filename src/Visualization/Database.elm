module Visualization.Database exposing (graph)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Table as Table
import Data.Point exposing (Point)
import Data.Size exposing (Size)
import Data.Table exposing (Table)
import Data.Database exposing (Database)
import Data.Modifier exposing (Modifier(..))


padding = 20
spacing = 50

link : Point -> Point -> Svg msg
link start end =
  g []
    [ circle [ cx <| String.fromFloat start.x, cy <| String.fromFloat start.y, r "3" ] []
    , circle [ cx <| String.fromFloat end.x, cy <| String.fromFloat end.y, r "3" ] []
    , Svg.path
      [ d <| "M " ++ String.fromFloat start.x ++ " " ++ String.fromFloat start.y
        ++ " C " ++ String.fromFloat (start.x + spacing / 2) ++ " " ++ String.fromFloat start.y
        ++ ", " ++ String.fromFloat (end.x + spacing / 2 * (if end.x > start.x then -1 else 1)) ++ " " ++ String.fromFloat end.y
        ++ ", " ++ String.fromFloat end.x ++ " " ++ String.fromFloat end.y
      , fill "none"
      , stroke "black"
      , strokeWidth "2"
      ]
      []
    ]

tablesPositions : List Table -> List (Point, Size, Table)
tablesPositions database =
  let
    sortedTables =
      List.sortBy
        (\table -> -(List.length table.fields))
        database
    maxHeight =
      Maybe.withDefault 0
        <| Maybe.map .height
        <| Maybe.map Table.boxSize
        <| List.head sortedTables
    computeY : Float -> Float -> Float
    computeY occupied height =
      if maxHeight >= occupied + spacing + height then
        occupied + spacing
      else
        0
  in
    ( List.foldl
      (\table acc ->
        let
          tableSize = Table.boxSize table
          y = computeY (acc.y - padding) tableSize.height
          newColumn = y == 0
        in
          { acc
          | x = (if newColumn then acc.x + acc.maxColumnWidth + spacing else acc.x)
          , y = (if newColumn then padding + tableSize.height else acc.y + tableSize.height)
          , maxColumnWidth = if newColumn then tableSize.width else Basics.max acc.maxColumnWidth tableSize.width
          , list =
            ( Point (if newColumn then acc.x + acc.maxColumnWidth + spacing else acc.x) (y + padding)
            , tableSize
            , table
            ) :: acc.list
          }
      )
      { x = padding - spacing, y = padding , maxColumnWidth = 0, list = [] }
      sortedTables
    ).list |> List.reverse

tables : List Table -> List (Svg msg)
tables database =
  List.map
    (\((position, _, table)) -> Table.box position table)
    (tablesPositions database)

findTable : List (Point, Size, Table) -> String -> Maybe (Point, Size, Table)
findTable database name =
  List.foldl
    (\(position, size, table) acc -> if table.name == name then Just (position, size, table) else acc)
    Nothing
    database

findFieldPosition : Table -> String -> Maybe Int
findFieldPosition table name =
  ( List.foldl
    (\field acc -> if field.name == name then { acc | position = Just acc.index, index = acc.index + 1 } else { acc | index = acc.index + 1 })
    { position = Nothing, index = 0 }
    table.fields
  ).position

links : List Table -> List (Svg msg)
links database =
  let
    tablesWithPositions = tablesPositions database
  in
    List.concat
      ( List.map
        (\(tablePosition, size, table) -> List.concat
          ( List.indexedMap
            (\fieldIndex field ->
              ( List.map
                (\modifier -> case modifier of
                  ForeignKey foreignTableName foreignField ->
                    case (findTable tablesWithPositions foreignTableName) of
                      Just (foreignPosition, foreignSize, foreignTable) ->
                        case findFieldPosition foreignTable foreignField of
                          Just foreignFieldPosition ->
                            let
                              fromX =
                                if (tablePosition.x > foreignPosition.x) then
                                  tablePosition.x
                                else
                                  tablePosition.x + size.width
                              toX =
                                if (tablePosition.x >= foreignPosition.x) then
                                  foreignPosition.x + foreignSize.width
                                else
                                  foreignPosition.x
                              start = Point fromX (tablePosition.y + (Table.fieldY fieldIndex) + padding)
                              end = Point toX (foreignPosition.y + (Table.fieldY foreignFieldPosition) + padding)
                            in
                              link start end
                          _ -> text ""
                      Nothing -> text ""
                  _ -> text ""
                )
                field.modifiers
              )
            )
            table.fields
          )
        )
        tablesWithPositions
      )

graph : Size -> Database -> Svg msg
graph {width, height} database =
  svg
    [ viewBox <| "0 0 " ++ (String.fromFloat width) ++ " " ++ (String.fromFloat height)
    , Svg.Attributes.style "background: linear-gradient(#f0f3f7, #e5eef6)"
    ]
    ( List.concat
      [ [ Svg.filter [ id "dropshadow", Svg.Attributes.height "130%" ]
          [ feGaussianBlur [ in_ "SourceAlpha", stdDeviation "6" ] []
          , feOffset [ dx "0", dy "7", result "offsetblur" ] []
          , feComponentTransfer []
            [ feFuncA [ type_ "linear", slope "0.1" ] []
            ]
          , feMerge []
            [ feMergeNode [] []
            , feMergeNode [ in_ "SourceGraphic" ] []
            ]
          ]
        ]
      , links database.tables
      , tables database.tables
      ]
    )
