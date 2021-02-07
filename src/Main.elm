module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import List.Extra
import Sample
import Set exposing (Set)
import Set.Extra
import String.Conversions as Conv
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Cell =
    ( Int, Int )


type alias Model =
    { cells : Set Cell
    , initCells : Set Cell
    , start : Bool
    , position : { x : Int, y : Int }
    , countGen : Int
    , invFrameSpeed : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cells = Set.empty
      , initCells = Set.empty
      , start = False
      , position = { x = 0, y = 0 }
      , countGen = 0
      , invFrameSpeed = 100
      }
    , Cmd.none
    )


type Msg
    = Move Time.Posix
    | Start
    | Stop
    | Reset
    | Clear
    | Add ( Float, Float )
    | ReadSample (Set Cell)
    | ChangeFrameSpeed String


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.start then
        Time.every model.invFrameSpeed Move

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move _ ->
            let
                surroundCells =
                    Set.Extra.concatMap createSurround model.cells

                nextCells =
                    Set.filter (isNextGen model.cells) surroundCells
            in
            if model.cells == nextCells then
                update Stop model

            else
                ( { model
                    | cells = nextCells
                    , countGen = model.countGen + 1
                  }
                , Cmd.none
                )

        Start ->
            ( { model | start = True }
            , Cmd.none
            )

        Stop ->
            ( { model | start = False }
            , Cmd.none
            )

        Reset ->
            ( { model | cells = model.initCells, countGen = 0 }
            , Cmd.none
            )

        Clear ->
            ( { model | cells = Set.empty, initCells = Set.empty, start = False, countGen = 0 }
            , Cmd.none
            )

        -- マス目をクリックしてcellを追加
        Add pos ->
            if model.start then
                ( model, Cmd.none )

            else
                let
                    div10 tuple =
                        Tuple.pair (floor (Tuple.first tuple / 10)) (floor (Tuple.second tuple / 10))

                    newCell =
                        div10 pos
                in
                -- クリックしたセルがすでに埋められている場合セルを削除
                if Set.member newCell model.cells then
                    ( { model
                        | cells = Set.remove newCell model.cells
                        , initCells = Set.remove newCell model.initCells
                      }
                    , Cmd.none
                    )

                else
                    ( { model
                        | cells = Set.insert newCell model.cells
                        , initCells = Set.insert newCell model.initCells
                      }
                    , Cmd.none
                    )

        ReadSample sample ->
            ( { model | cells = sample, initCells = sample }
            , Cmd.none
            )

        ChangeFrameSpeed invS ->
            ( { model | invFrameSpeed = Maybe.withDefault 100 <| String.toFloat invS }
            , Cmd.none
            )



-- `cell`の周りのセルを生成


createSurround : Cell -> Set Cell
createSurround cell =
    let
        x =
            Tuple.first cell

        y =
            Tuple.second cell
    in
    Set.fromList
        [ ( x - 1, y - 1 )
        , ( x, y - 1 )
        , ( x + 1, y - 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        , ( x - 1, y + 1 )
        , ( x, y + 1 )
        , ( x + 1, y + 1 )
        ]



-- `cell`が次に生き残れるか


isNextGen : Set Cell -> Cell -> Bool
isNextGen livingCells cell =
    let
        c =
            countSurround cell livingCells
    in
    case c of
        2 ->
            Set.member cell livingCells

        3 ->
            True

        _ ->
            False



-- `cell`の周りを数える


countSurround : Cell -> Set Cell -> Int
countSurround cell livingCells =
    let
        x =
            Tuple.first cell

        y =
            Tuple.second cell
    in
    List.Extra.count (\c -> Set.member c livingCells) <|
        [ ( x - 1, y - 1 )
        , ( x, y - 1 )
        , ( x + 1, y - 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        , ( x - 1, y + 1 )
        , ( x, y + 1 )
        , ( x + 1, y + 1 )
        ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] (operations model)
        , Html.div [ Mouse.onClick (.offsetPos >> Add) ] [ viewCells model.cells ]
        , Html.div [] [ Html.text <| "generation: " ++ String.fromInt model.countGen ]
        , Html.div [] [ Html.text <| Conv.fromSet (Conv.fromTuple2 String.fromInt String.fromInt) model.cells ]
        ]


viewCells : Set Cell -> Html msg
viewCells cells =
    let
        width =
            191

        height =
            83

        realW =
            String.fromInt (width * 10)

        realH =
            String.fromInt (height * 10)
    in
    Svg.svg
        [ SA.viewBox <| "0 0 " ++ realW ++ " " ++ realH
        , SA.width "100%"
        , SA.height "100%"
        ]
        (showCells <| List.map makeSvgCell (Set.toList cells))


operations : Model -> List (Html Msg)
operations model =
    [ Html.button
        [ HE.onClick <|
            if model.start then
                Stop

            else
                Start
        ]
        [ Html.text <|
            if model.start then
                "Stop"

            else
                "Run"
        ]
    , Html.button [ HE.onClick Reset ] [ Html.text "Reset" ]
    , Html.button [ HE.onClick Clear ] [ Html.text "Clear" ]
    , Html.button [ HE.onClick (Move (Time.millisToPosix 0)) ] [ Html.text "Move" ]
    , Html.select []
        [ Html.option [ HE.onClick (ReadSample Set.empty) ] [ Html.text "Empty" ]
        , Html.option [ HE.onClick (ReadSample Sample.gliderGun) ] [ Html.text "GliderGun" ]
        , Html.option [ HE.onClick (ReadSample Sample.galaxy) ] [ Html.text "Galaxy" ]
        , Html.option [ HE.onClick (ReadSample Sample.glider) ] [ Html.text "Glider" ]
        , Html.option [ HE.onClick (ReadSample Sample.spaceships) ] [ Html.text "Spaceships" ]
        , Html.option [ HE.onClick (ReadSample Sample.makeSpaceship) ] [ Html.text "Make Spaceship" ]
        , Html.option [ HE.onClick (ReadSample Sample.ship60P5H2V0) ] [ Html.text "60P5H2V0" ]
        , Html.option [ HE.onClick (ReadSample Sample.shuttle) ] [ Html.text "Shuttle" ]
        , Html.option [ HE.onClick (ReadSample Sample.acorn) ] [ Html.text "Acorn" ]
        ]
    , Html.input
        [ HA.type_ "number"
        , HA.size 1
        , HA.value <| String.fromFloat model.invFrameSpeed
        , HE.onInput ChangeFrameSpeed
        ]
        []
    ]


makeSvgCell : Cell -> Svg msg
makeSvgCell cell =
    Svg.rect
        [ SA.x <| String.fromInt <| 10 * Tuple.first cell
        , SA.y <| String.fromInt <| 10 * Tuple.second cell
        , SA.width "10"
        , SA.height "10"
        , SA.fill "black"
        ]
        []


showCells : List (Svg msg) -> List (Svg msg)
showCells rects =
    [ Svg.defs []
        [ -- gridを描く
          Svg.pattern
            [ SA.id "patterngrid"
            , SA.width "10"
            , SA.height "10"
            , SA.patternUnits "userSpaceOnUse"
            ]
            [ Svg.path
                [ SA.d "M 0 0 H 10 V 10"
                , SA.fill "none"
                , SA.stroke "gray"
                , SA.strokeWidth "0.5"
                ]
                []
            ]
        , Svg.rect
            [ SA.id "grid"
            , SA.width "100%"
            , SA.height "100%"
            , SA.fill "url(#patterngrid)"
            ]
            []
        , Svg.symbol [ SA.id "cells" ]
            (Svg.use [ SA.xlinkHref "#grid" ] [] :: rects)
        ]
    , Svg.use [ SA.xlinkHref "#cells" ] []
    ]
