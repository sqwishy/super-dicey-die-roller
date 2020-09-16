module Roll exposing (main, view)

import Browser
import Html exposing (Html, button, div, em, input, span, text)
import Html.Attributes
    exposing
        ( class
        , classList
        , disabled
        , name
        , placeholder
        , style
        , value
        )
import Html.Events exposing (onClick, onInput)
import NumberToWords exposing (intToWords)
import Parser exposing ((|.), (|=), Parser)
import Parser.Advanced as ParserA
import Random


mapEmpty : String -> String -> String
mapEmpty from to =
    if String.isEmpty from then
        to

    else
        from


resultIsErr : Result a b -> Bool
resultIsErr res =
    case res of
        Ok _ ->
            False

        Err _ ->
            True


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


rollSumStr : Roll -> Int -> String
rollSumStr roll sum =
    case roll.die of
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
        |. Parser.spaces
        |= Parser.map (Maybe.withDefault 1) parseCount
        |= parseDie
        |. Parser.spaces
        |. Parser.end


parseCount : Parser (Maybe Int)
parseCount =
    Parser.succeed ()
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                if String.isEmpty s then
                    Parser.succeed Nothing

                else
                    case String.toInt s of
                        Just int ->
                            Parser.succeed (Just int)

                        Nothing ->
                            ParserA.problem Parser.ExpectingInt
            )


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
                        Parser.map (\m -> Sided (Maybe.withDefault 6 m)) parseCount

                    _ ->
                        Parser.problem "try \"f\" or \"d\""
            )


type alias InputErr =
    List Parser.DeadEnd


type alias RolledDie =
    { face : Int, timing : Int }


type alias Model =
    { roll : Roll
    , text : String
    , dice : List RolledDie
    , result : Result InputErr ()
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
    if String.isEmpty s then
        ( { model | text = s, result = Ok () }, Cmd.none )

    else
        case Parser.run parseRoll s of
            Ok roll ->
                ( { model | text = s, result = Ok () }, rerollCmd roll )

            Err e ->
                ( { model | text = s, result = Err e }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    newModel |> modelForText "4f"


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
    Browser.document
        { init = init
        , subscriptions = \x -> Sub.none
        , update = update
        , view = viewDoc
        }


viewDoc : Model -> Browser.Document Msg
viewDoc model =
    { title = "Tiny Dice Roller Button"
    , body = [ view model ]
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


type Outcome
    = Whatever
    | Good
    | Bad


outcomeClass : Outcome -> String
outcomeClass outcome =
    case outcome of
        Whatever ->
            ""

        Good ->
            "yay"

        Bad ->
            "nay"


outcomeForRange : { hi : Int, lo : Int } -> Int -> Outcome
outcomeForRange range n =
    if n == range.lo then
        Bad

    else if n == range.hi then
        Good

    else
        Whatever


rollFaceStyle : Die -> Int -> Outcome
rollFaceStyle die face =
    case die of
        Fate ->
            Whatever

        Sided s ->
            if s >= 4 then
                outcomeForRange { lo = 1, hi = s } face

            else
                Whatever


rollSumStyle : Roll -> Int -> Outcome
rollSumStyle roll sum =
    case roll.die of
        Fate ->
            if roll.count >= 4 then
                outcomeForRange { lo = -roll.count, hi = roll.count } sum

            else
                Whatever

        Sided _ ->
            Whatever


view : Model -> Html Msg
view model =
    let
        sum =
            rollSum model.dice
    in
    div [ class "die-roller", class (reanimateClass model.reanimate), class (resultClass model.result) ]
        [ viewDiceRoll model.roll model.dice
        , div [ class "roller-input" ]
            [ input [ onInput WithDice, value model.text, placeholder "try 2d d20 or 4f" ] []
            , button [ onClick ReRoll, name "re-roll", disabled (resultIsErr model.result) ]
                [ span [ class "re-roll-icon" ] [ text "🎲" ]
                , span [ class "roll-sum-sign" ] [ text "»" ]
                , span [ class "roll-sum", class (rollSumStyle model.roll sum |> outcomeClass) ]
                    [ rollSumStr model.roll sum |> text ]
                ]
            ]
        , viewResult model.text (Result.map (\_ -> model.roll) model.result)
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
    span
        [ class "roll-face"
        , style "animation-duration" (timingStyle die)
        , class (rollFaceStyle roll.die die.face |> outcomeClass)
        ]
        [ text (dieFaceTxt roll.die die.face) ]


viewRollSum : Roll -> Int -> Html Msg
viewRollSum roll sum =
    span
        [ class "roll-sum"
        , class (rollSumStyle roll sum |> outcomeClass)
        ]
        [ text (rollSumStr roll sum) ]


viewResult : String -> Result InputErr Roll -> Html msg
viewResult input res =
    let
        children =
            case res of
                Ok roll ->
                    summarizeRoll roll

                Err err ->
                    viewInputErr input err
    in
    div [ class "input-result" ] children



{-
   roll n fate die
   roll n die with n side each
   roll n dice with n sides each
-}


summarizeRoll : Roll -> List (Html msg)
summarizeRoll roll =
    [ text "roll "
    , em [] [ text (intToWords roll.count) ]
    , text " "
    ]
        ++ (case roll.die of
                Fate ->
                    [ text "fate ", text (pluralize roll.count words.dice) ]

                Sided sides ->
                    [ text (pluralize roll.count words.dice)
                    , text " with "
                    , em [] [ text (intToWords sides) ]
                    , text " "
                    , text (pluralize sides words.sides)
                    , text (pluralize roll.count { singular = "", plural = " each" })
                    ]
           )


pluralize : Int -> { singular : a, plural : a } -> a
pluralize n word =
    if n == 1 then
        word.singular

    else
        word.plural


words =
    { dice = { singular = "die", plural = "dice" }
    , sides = { singular = "side", plural = "sides" }
    }


viewInputErr : String -> InputErr -> List (Html msg)
viewInputErr input errList =
    errList
        |> List.map (viewDeadEnd input)
        |> List.foldr (++) []


type alias MarkedInput =
    { head : String
    , bad : String
    , tail : String
    }


markedInput : String -> ( Int, Int ) -> MarkedInput
markedInput input ( row, col ) =
    { head = input |> String.left (col - 1)
    , bad = input |> String.dropLeft (col - 1) |> String.left 1
    , tail = input |> String.dropLeft col
    }


suggestionForProblem : MarkedInput -> Parser.Problem -> String
suggestionForProblem marked problem =
    case problem of
        Parser.ExpectingInt ->
            if String.isEmpty marked.bad then
                "type a number"

            else
                "expected a number"

        Parser.Problem s ->
            s

        Parser.ExpectingEnd ->
            "can't parse this extra stuff"

        _ ->
            "this input is weird"


viewDeadEnd : String -> Parser.DeadEnd -> List (Html msg)
viewDeadEnd input err =
    let
        marked =
            markedInput input ( err.row, err.col )
    in
    [ span [ class "annotated-input" ]
        [ span [] [ text marked.head ]
        , span [ class "bad-input" ] [ text (mapEmpty marked.bad "\u{2002}") ]
        , span [] [ text marked.tail ]
        ]
    , span
        [ class "suggestion" ]
        [ text "« "
        , text (suggestionForProblem marked err.problem)
        ]
    ]
