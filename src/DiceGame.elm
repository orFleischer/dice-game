port module DiceGame exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes as Svg exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, visibility, width, x, y)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


configs =
    { diceColors = { red = "#EB1124", white = "#f5f5f5" }
    , animationFrameDelay = 100
    , throwsPerGame = 5
    , diceRollsPerGame = 10
    }


type alias Model =
    { dieFace1 : DiceFace
    , dieFace2 : DiceFace
    , numOfRolls : Int
    , isRolling : Bool
    , hasWon : Bool
    , throwsLeft : Int
    , score : Int
    , hiScores : List HiScore
    }


type alias HiScore =
    { score : Int
    , name : String
    }


type DiceFace
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type Msg
    = Roll
    | ContinueRoll
    | PromptUser
    | GotUserName (Maybe String)
    | NewFace DiceFace DiceFace


init : Encode.Value -> ( Model, Cmd Msg )
init possibleSavedScore =
    case Decode.decodeValue hiScoresDecoder possibleSavedScore of
        Ok hiScores ->
            ( Model One One 0 False False configs.throwsPerGame 0 hiScores, Cmd.none )

        Err _ ->
            ( Model One One 0 False False configs.throwsPerGame 0 [], Cmd.none )



-- PORTs


port setStorage : Encode.Value -> Cmd msg


port promptUser : String -> Cmd msg


port getUserName : (Maybe String -> msg) -> Sub msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        dices : Random.Generator Msg
        dices =
            Random.map2 (\dice1 dice2 -> NewFace dice1 dice2) (Random.uniform One [ Two, Three, Four, Five, Six ]) (Random.uniform One [ Two, Three, Four, Five, Six ])
    in
    case msg of
        Roll ->
            ( { model | numOfRolls = 0, isRolling = True, hasWon = False }
            , Random.generate identity dices
            )

        NewFace newFace1 newFace2 ->
            ( Model newFace1 newFace2 model.numOfRolls model.isRolling model.hasWon model.throwsLeft model.score model.hiScores
            , Cmd.none
            )

        PromptUser ->
            ( model, promptUser "Enter you initials" )

        GotUserName maybeName ->
            let
                effectiveName =
                    Maybe.withDefault "hatul" maybeName

                newHiScore =
                    HiScore model.score effectiveName

                newHiScores : List HiScore
                newHiScores =
                    (newHiScore :: model.hiScores)
                        |> List.sortBy .score
                        |> List.reverse
                        |> List.take 2

                _ =
                    Debug.log "encoded stuff" newHiScores

                newModel =
                    Model One One 0 False False configs.throwsPerGame 0 newHiScores
            in
            ( newModel, setStorage <| encodeHiScores newHiScores )

        ContinueRoll ->
            if model.numOfRolls <= configs.diceRollsPerGame then
                ( { model | numOfRolls = model.numOfRolls + 1 }
                , Random.generate identity dices
                )

            else
                let
                    hasWon =
                        model.dieFace2 == model.dieFace1

                    newScore =
                        model.score
                            + (if hasWon then
                                faceScore model.dieFace1

                               else
                                0
                              )

                    newModel : Model
                    newModel =
                        { model | numOfRolls = 0, isRolling = False, hasWon = hasWon, score = newScore, throwsLeft = model.throwsLeft - 1 }
                in
                if newModel.throwsLeft <= 0 then
                    update PromptUser newModel

                else
                    ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isRolling then
        Sub.batch [ getUserName GotUserName, Time.every configs.animationFrameDelay (\_ -> ContinueRoll) ]

    else
        Sub.batch [ getUserName GotUserName ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "pure-g" ]
        [ div [ class "pure-u-1-3" ]
            [ div []
                [ h1 [] [ text (String.fromInt (faceValue model.dieFace1)) ]
                , svg [] (dice 10 10 50 model.dieFace1)
                ]
            , div []
                [ h1 [] [ text (String.fromInt (faceValue model.dieFace2)) ]
                , svg [] (dice 10 10 50 model.dieFace2)
                ]
            , button [ onClick Roll, disabled model.isRolling ] [ div [ class "roll-button" ] [ div [] [ text "Roll" ], div [] [ text "Roll" ] ] ]
            , h1 [] [ renderWinLoss model ]
            ]
        , div [ class "pure-u-1-3" ] [ h1 [] [ text ("Your Score is " ++ String.fromInt model.score) ] ]
        , div [ class "pure-u-1-3" ] (renderHiScores model.hiScores)
        ]


renderHiScores : List HiScore -> List (Html msg)
renderHiScores hiScores =
    case hiScores of
        [] ->
            []

        nonEmptyHiScores ->
            let
                renderedHiScores =
                    nonEmptyHiScores
                        |> List.map (\{ score, name } -> name ++ "   " ++ String.fromInt score)
                        |> List.map (\scoreStr -> li [] [ text scoreStr ])
            in
            [ h1 [] [ text "High Scores" ]
            , ol [] renderedHiScores
            ]


renderWinLoss : Model -> Html msg
renderWinLoss model =
    if not model.isRolling then
        if model.hasWon then
            text "won!"

        else
            text "lost!"

    else
        text " rolling"


dice : Float -> Float -> Float -> DiceFace -> List (Svg msg)
dice upperLeftX upperLeftY edgeLength diceFace =
    let
        edgeLengthStr =
            String.fromFloat edgeLength

        rectSvg =
            rect
                [ width edgeLengthStr
                , height edgeLengthStr
                , x <| String.fromFloat upperLeftX
                , y <| String.fromFloat upperLeftY
                , rx "2"
                , ry "2"
                , fill configs.diceColors.red
                , stroke "black"
                , strokeWidth "2"
                ]
                []
    in
    rectSvg :: diceDots upperLeftX upperLeftY edgeLength diceFace


diceDots : Float -> Float -> Float -> DiceFace -> List (Svg msg)
diceDots x y edgeLength diceFace =
    let
        diceRenderer =
            \f -> f x y edgeLength
    in
    case diceFace of
        One ->
            List.map diceRenderer [ diceDotCenter ]

        Two ->
            List.map diceRenderer [ diceDotLowerLeft, diceDotUpperRight ]

        Three ->
            List.map diceRenderer [ diceDotLowerLeft, diceDotUpperRight, diceDotCenter ]

        Four ->
            List.map diceRenderer [ diceDotLowerLeft, diceDotLowerRight, diceDotUpperLeft, diceDotUpperRight ]

        Five ->
            List.map diceRenderer [ diceDotLowerLeft, diceDotLowerRight, diceDotUpperLeft, diceDotUpperRight, diceDotCenter ]

        Six ->
            List.map diceRenderer [ diceDotLowerLeft, diceDotLowerRight, diceDotUpperLeft, diceDotUpperRight, diceDotMiddleLeft, diceDotMiddleRight ]


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
        [ r <| String.fromFloat radius
        , cx <| String.fromFloat centerX
        , cy <| String.fromFloat centerY
        , fill configs.diceColors.white
        , Svg.strokeWidth "0.5"
        , Svg.stroke "black"
        ]
        []



-- Encoders/Decoders


hiScoresDecoder : Decode.Decoder (List HiScore)
hiScoresDecoder =
    Decode.field "hi_scores" (Decode.list hiScoreDecoder)


hiScoreDecoder : Decode.Decoder HiScore
hiScoreDecoder =
    Decode.map2 HiScore (Decode.field "score" Decode.int) (Decode.field "name" Decode.string)


encodeHiScores : List HiScore -> Encode.Value
encodeHiScores hiScores =
    Encode.object [ ( "hi_scores", Encode.list encodeHiScore hiScores ) ]


encodeHiScore : HiScore -> Encode.Value
encodeHiScore hiScore =
    Encode.object [ ( "score", Encode.int hiScore.score ), ( "name", Encode.string hiScore.name ) ]


faceScore : DiceFace -> Int
faceScore diceFace =
    faceValue diceFace


faceValue : DiceFace -> Int
faceValue diceFace =
    case diceFace of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6
