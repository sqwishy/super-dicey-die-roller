module Roll exposing (main, view)

import Browser
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, classList, name, style, value)
import Html.Events exposing (onClick, onInput)
import Parser exposing ((|.), (|=), Parser)
import Random


type Die
    = Fate
    | Sided Int


type alias Roll =
    { count : Int, die : Die }


dieRoll die =
    case die of
        Fate ->
            Random.int -1 1

        Sided s ->
            Random.int 1 s


dieFaceTxt : Die -> Int -> String
dieFaceTxt die face =
    case die of
        Fate ->
            if face == 1 then
                "+"

            else if face == 0 then
                ""

            else if face == -1 then
                "-"

            else
                String.fromInt face

        Sided _ ->
            String.fromInt face


rollSumTxt : Die -> Int -> String
rollSumTxt die sum =
    case die of
        Fate ->
            if sum > 0 then
                "+" ++ String.fromInt sum

            else
                String.fromInt sum

        Sided _ ->
            String.fromInt sum


generateRolls : Roll -> Random.Generator (List Int)
generateRolls roll =
    Random.list roll.count (dieRoll roll.die)


parseRoll : Parser Roll
parseRoll =
    Parser.succeed Roll
        |= Parser.int
        |= parseDie


parseDie : Parser Die
parseDie =
    Parser.succeed ()
        |. Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                case String.toLower s of
                    "f" ->
                        Parser.succeed Fate

                    "df" ->
                        Parser.succeed Fate

                    "d" ->
                        Parser.map Sided Parser.int

                    _ ->
                        Parser.problem "expected f or d<n>"
            )


type alias RolledDie =
    { face : Int, timing : Int }


type alias Model =
    { roll : Roll
    , text : String
    , dice : List RolledDie
    , result : Result String ()
    , reanimate : Bool
    }


type Msg
    = WithDice String
    | GotRolls Roll (List RolledDie)
    | ReRoll


rollFaceTimings : Roll -> Random.Generator (List Int)
rollFaceTimings roll =
    Random.list roll.count (Random.int 160 280)


rerollCmd : Roll -> Cmd Msg
rerollCmd roll =
    Random.generate (\( a, b ) -> List.map2 RolledDie a b |> GotRolls roll)
        (Random.pair
            (generateRolls roll)
            (rollFaceTimings roll)
        )


newModel : Model
newModel =
    { text = ""
    , roll = Roll 0 Fate
    , dice = []
    , result = Ok ()
    , reanimate = False
    }


modelForText : String -> Model -> ( Model, Cmd Msg )
modelForText s model =
    case Parser.run parseRoll s of
        Ok roll ->
            ( { model | text = s, result = Ok () }, rerollCmd roll )

        Err e ->
            ( { model | text = s, result = Err (Debug.toString e) }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    newModel |> modelForText "4df"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRolls roll dice ->
            ( { model | roll = roll, dice = dice, reanimate = not model.reanimate }, Cmd.none )

        WithDice s ->
            modelForText s model

        ReRoll ->
            ( model, rerollCmd model.roll )


main =
    Browser.element
        { init = init
        , subscriptions = \x -> Sub.none
        , update = update
        , view = view
        }


resultClass res =
    case res of
        Ok _ ->
            "ok"

        Err _ ->
            "err"


reanimateClass b =
    if b then
        "wiggle"

    else
        "waggle"


timingStyle die =
    String.fromInt die.timing ++ "ms"


rollSum : List RolledDie -> Int
rollSum dice =
    dice |> List.map (\d -> d.face) |> List.sum


view : Model -> Html Msg
view model =
    div [ class "die-roller", class (reanimateClass model.reanimate), class (resultClass model.result) ]
        [ viewDiceRoll model.roll model.dice
        , viewResult model.result
        , div [ class "roller-input" ]
            [ input [ onInput WithDice, value model.text ] []
            , button [ onClick ReRoll, name "re-roll" ]
                [ span [ class "re-roll-icon" ] [ text "🎲" ]
                , span [ class "roll-sum-sign" ] [ text "=" ]
                , span [ class "roll-sum" ]
                    [ text (rollSumTxt model.roll.die (rollSum model.dice))
                    ]
                ]
            ]
        ]


viewDiceRoll : Roll -> List RolledDie -> Html Msg
viewDiceRoll roll dice =
    let
        items =
            List.map (viewRollFace roll) dice
    in
    div [ class "dice-roll" ] items


viewRollFace : Roll -> RolledDie -> Html Msg
viewRollFace roll die =
    span [ class "roll-face", style "animation-duration" (timingStyle die) ]
        [ text (dieFaceTxt roll.die die.face) ]


viewRollSum : Roll -> Int -> Html Msg
viewRollSum roll sum =
    span [ class "roll-sum" ] [ text (rollSumTxt roll.die sum) ]


viewResult : Result String () -> Html Msg
viewResult res =
    case res of
        Ok () ->
            div [ class "input-result" ] []

        Err e ->
            div [ class "input-result result-e.2rror" ] [ text e ]
