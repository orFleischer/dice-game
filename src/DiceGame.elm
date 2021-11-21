port module DiceGame exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Random
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes as Svg exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, width, x, y)
import Time
import Json.Decode as Decode
import Json.Encode as Encode


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


diceColors =
    { red = "#EB1124", white = "white" }



-- MODEL


type alias Model =
    { dieFace1 : Int
    , dieFace2 : Int
    , numOfRolls : Int
    , isRolling : Bool
    , hasWon : Bool
    , score : Int
    , hiScores : List HiScore
    }

type alias HiScore =
     { score : Int
     , name : String
     }


{-init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1 1 0 False False 0
    , Cmd.none
    )-}


init : Encode.Value -> ( Model, Cmd Msg )
init possibleSavedScore =
    let
        _ = Debug.log "saved score is " possibleSavedScore
    in
    case Decode.decodeValue hiScoresDecoder possibleSavedScore of
        Ok hiScores ->
            ( Model 1 1 0 False False 0 hiScores, Cmd.none)
        Err _ ->
            ( Model 1 1 0 False False 0 [], Cmd.none)

{-    ( Model 1 1 0 False False (Maybe.withDefault 0 possibleSavedScore)
    , Cmd.none
    )-}



-- UPDATE


type Msg
    = Roll
    | ContinueRoll
    | NewFace Int Int


port setStorage : Int -> Cmd msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        dices : Random.Generator Msg
        dices =
            Random.map2 (\dice1 dice2 -> NewFace dice1 dice2) (Random.int 1 6) (Random.int 1 6)
    in
    case msg of
        Roll ->
            ( { model | numOfRolls = 0, isRolling = True, hasWon = False }
            , Random.generate identity dices
            )

        NewFace newFace1 newFace2 ->
            ( Model newFace1 newFace2 model.numOfRolls model.isRolling model.hasWon model.score model.hiScores
            , Cmd.none
            )

        ContinueRoll ->
            if model.numOfRolls <= 10 then
                ( { model | numOfRolls = model.numOfRolls + 1 }
                , Random.generate identity dices
                )

            else
                let
                    hasWon =
                        model.dieFace2 == model.dieFace1

                    score =
                        model.score
                            + (if hasWon then
                                1

                               else
                                0
                              )

                    _ =
                        Debug.log "score is " score
                in
                ( { model | numOfRolls = 0, isRolling = False, hasWon = hasWon, score = score }
                , Cmd.batch [setStorage score]--Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isRolling then
        Time.every 100 (\_ -> ContinueRoll)

    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style "float" "left" ]
            [ div []
                [ h1 [] [ text (String.fromInt model.dieFace1) ]
                , svg [] (dice 10 10 50 model.dieFace1)
                ]
            , div []
                [ h1 [] [ text (String.fromInt model.dieFace2) ]
                , svg [] (dice 10 10 50 model.dieFace2)
                ]
            , button [ onClick Roll, disabled model.isRolling ] [ text "Roll" ]
            , h1 [] [ renderWinLoss model ]
            ]
        , div []
            [ h1 [] [ text ("Your Score is " ++ (String.fromInt model.score)) ]
            , h1 [] [text "High Scores"]
            , ol [] (renderHiScores model.hiScores)

            ]

        ]

renderHiScores: List HiScore -> List (Html msg)
renderHiScores hiScores =
    hiScores
        |> List.map (\{score, name} -> name ++ "   " ++ String.fromInt score)
        |> List.map (\scoreStr -> li [] [text scoreStr])



renderWinLoss : Model -> Html msg
renderWinLoss model =
    if not model.isRolling then
        if model.hasWon then
            text "won!"

        else
            text "lost!"

    else
        text " rolling"


dice : Float -> Float -> Float -> Int -> List (Svg msg)
dice upperLeftX upperLeftY edgeLength diceFace =
    let
        edgeLengthStr =
            String.fromFloat edgeLength

        rectSvg =
            rect
                [ width edgeLengthStr
                , height edgeLengthStr
                , upperLeftX |> String.fromFloat |> x
                , upperLeftY |> String.fromFloat |> y
                , rx "2"
                , ry "2"
                , fill diceColors.red
                , stroke "black"
                , strokeWidth "2"
                ]
                []
    in
    rectSvg :: diceDots upperLeftX upperLeftY edgeLength diceFace


diceDots : Float -> Float -> Float -> Int -> List (Svg msg)
diceDots x y edgeLength diceFace =
    case diceFace of
        1 ->
            List.map (\f -> f x y edgeLength) [ diceDotCenter ]

        2 ->
            List.map (\f -> f x y edgeLength) [ diceDotLowerLeft, diceDotUpperRight ]

        3 ->
            List.map (\f -> f x y edgeLength) [ diceDotLowerLeft, diceDotUpperRight, diceDotCenter ]

        4 ->
            List.map (\f -> f x y edgeLength) [ diceDotLowerLeft, diceDotLowerRight, diceDotUpperLeft, diceDotUpperRight ]

        5 ->
            List.map (\f -> f x y edgeLength) [ diceDotLowerLeft, diceDotLowerRight, diceDotUpperLeft, diceDotUpperRight, diceDotCenter ]

        6 ->
            List.map (\f -> f x y edgeLength) [ diceDotLowerLeft, diceDotLowerRight, diceDotUpperLeft, diceDotUpperRight, diceDotMiddleLeft, diceDotMiddleRight ]

        _ ->
            List.map (\f -> f x y edgeLength) [ diceDotLowerLeft, diceDotLowerRight, diceDotUpperLeft, diceDotUpperRight, diceDotMiddleLeft, diceDotMiddleRight ]


diceDotUpperLeft : Float -> Float -> Float -> Svg msg
diceDotUpperLeft diceX diceY diceEdgeLength =
    let
        radius =
            diceEdgeLength / 10

        centerX =
            diceX + radius + 2

        centerY =
            diceY + radius + 2
    in
    diceDot radius centerX centerY


diceDotLowerRight : Float -> Float -> Float -> Svg msg
diceDotLowerRight diceX diceY diceEdgeLength =
    let
        radius =
            diceEdgeLength / 10

        centerX =
            diceX + diceEdgeLength - 2 - radius

        centerY =
            diceY + diceEdgeLength - 2 - radius
    in
    diceDot radius centerX centerY


diceDotLowerLeft : Float -> Float -> Float -> Svg msg
diceDotLowerLeft diceX diceY diceEdgeLength =
    let
        radius =
            diceEdgeLength / 10

        centerX =
            diceX + radius + 2

        centerY =
            diceY + diceEdgeLength - 2 - radius
    in
    diceDot radius centerX centerY


diceDotCenter : Float -> Float -> Float -> Svg msg
diceDotCenter diceX diceY diceEdgeLength =
    let
        radius =
            diceEdgeLength / 10

        centerX =
            diceX + (diceEdgeLength / 2)

        centerY =
            diceY + (diceEdgeLength / 2)
    in
    diceDot radius centerX centerY


diceDotUpperRight : Float -> Float -> Float -> Svg msg
diceDotUpperRight diceX diceY diceEdgeLength =
    let
        radius =
            diceEdgeLength / 10

        centerX =
            diceX + diceEdgeLength - 2 - radius

        centerY =
            diceY + radius + 2
    in
    diceDot radius centerX centerY


diceDotMiddleLeft : Float -> Float -> Float -> Svg msg
diceDotMiddleLeft diceX diceY diceEdgeLength =
    let
        radius =
            diceEdgeLength / 10

        centerX =
            diceX + radius + 2

        centerY =
            diceY + (diceEdgeLength / 2)
    in
    diceDot radius centerX centerY


diceDotMiddleRight : Float -> Float -> Float -> Svg msg
diceDotMiddleRight diceX diceY diceEdgeLength =
    let
        radius =
            diceEdgeLength / 10

        centerX =
            diceX + diceEdgeLength - 2 - radius

        centerY =
            diceY + (diceEdgeLength / 2)
    in
    diceDot radius centerX centerY


diceDot : Float -> Float -> Float -> Svg msg
diceDot radius centerX centerY =
    circle
        [ radius |> String.fromFloat |> r
        , centerX |> String.fromFloat |> cx
        , centerY |> String.fromFloat |> cy
        , fill diceColors.white
        , Svg.strokeWidth "0.5"
        , Svg.stroke "black"
        ]
        []

hiScoresDecoder: Decode.Decoder (List HiScore)
hiScoresDecoder = Decode.field "hi_scores" (Decode.list hiScoreDecoder)

hiScoreDecoder: Decode.Decoder HiScore
hiScoreDecoder = Decode.map2 HiScore (Decode.field "score" Decode.int) (Decode.field "name" Decode.string)
