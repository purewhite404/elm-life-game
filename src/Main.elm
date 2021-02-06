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
import String.Conversions as SC
import Svg exposing (Svg, svg)
import Svg.Attributes as SA exposing (viewBox)
import Time


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
            if Set.isEmpty model.cells then
                update Stop model

            else
                let
                    surroundCells =
                        Set.Extra.concatMap createSurround model.cells
                in
                ( { model
                    | cells = Set.filter (isNextGen model.cells) surroundCells
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
        [ Html.div [ Mouse.onClick (.clientPos >> Add) ]
            [ let
                allCells =
                    List.Extra.lift2 Tuple.pair (List.range 0 79) (List.range 0 79)

                size =
                    String.fromFloat <| sqrt (toFloat <| List.length allCells) * 10
              in
              svg
                [ viewBox ("0 0 " ++ size ++ " " ++ size)
                , SA.width size
                , SA.height size
                ]
                (List.map showGrid allCells ++ List.map showCell (Set.toList model.cells))
            ]
        , Html.div []
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
                        "Start"
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
                ]
            , Html.input
                [ HA.type_ "number"
                , HA.size 1
                , HA.value <| String.fromFloat model.invFrameSpeed
                , HE.onInput ChangeFrameSpeed
                ]
                []
            , Html.div [] [ Html.text <| "generation: " ++ String.fromInt model.countGen ]
            , Html.div [] [ Html.text <| SC.fromSet (SC.fromTuple2 String.fromInt String.fromInt) model.cells ]
            ]
        ]


showGrid : Cell -> Svg msg
showGrid cell =
    Svg.rect
        [ SA.x <| String.fromInt <| 10 * Tuple.first cell
        , SA.y <| String.fromInt <| 10 * Tuple.second cell
        , SA.width "10"
        , SA.height "10"
        , SA.fill "white"
        , SA.stroke "gray"
        , SA.strokeWidth "0.4"
        ]
        []


showCell : Cell -> Svg msg
showCell cell =
    Svg.rect
        [ SA.x <| String.fromInt <| 10 * Tuple.first cell
        , SA.y <| String.fromInt <| 10 * Tuple.second cell
        , SA.width "10"
        , SA.height "10"
        , SA.fill "black"
        ]
        []
