module Main exposing (..)

import Ball exposing (getBall)
import Browser
import Config exposing (config, odds)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import List.Extra exposing (elemIndex, find, getAt, inits)
import Process
import Random
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Combination =
    List Int


type alias Player =
    { id : Int
    , name : String
    , combination : Combination
    , credit : Int
    }



-- MODEL


type alias Form =
    { name : String
    , combination : Combination
    }


type alias Model =
    { isPlaying : Bool
    , lastDrawn : Maybe Int
    , players : List Player
    , combination : Combination
    , form : Form
    }


initialForm : Form
initialForm =
    { name = ""
    , combination = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { isPlaying = False
      , lastDrawn = Nothing
      , players = []
      , combination = []
      , form = initialForm
      }
    , Cmd.none
    )


generateRandomNumber : (Int -> Msg) -> Int -> Int -> Cmd Msg
generateRandomNumber msg from to =
    Random.generate msg (Random.int from to)


postpone : Float -> Msg -> Cmd Msg
postpone time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)



-- UPDATE


type Msg
    = StartGame
    | GameCombination Int
    | AddPlayer
    | SetPlayerName String
    | PlayerId Int
    | AddToPlayerCombination Int
    | GameInProgress
    | GameIsOver


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlayer ->
            ( model, generateRandomNumber PlayerId 1 1000000 )

        PlayerId id ->
            let
                newPlayer : Player
                newPlayer =
                    { id = id
                    , name = model.form.name
                    , combination = model.form.combination
                    , credit = config.initialPlayerCredit
                    }
            in
            ( { model | players = newPlayer :: model.players, form = initialForm }, Cmd.none )

        StartGame ->
            let
                playerCredit : List Player
                playerCredit =
                    model.players
                        |> List.filter (\p -> p.credit >= config.bet)
                        |> List.map (\p -> { p | credit = p.credit - config.bet })
            in
            ( { model | players = playerCredit }, generateRandomNumber GameCombination 1 48 )

        GameCombination number ->
            if List.length model.combination < 35 then
                if List.member number model.combination then
                    ( model, generateRandomNumber GameCombination 1 48 )

                else
                    ( { model | combination = number :: model.combination }, generateRandomNumber GameCombination 1 48 )

            else
                ( { model | combination = model.combination, isPlaying = True }, postpone config.gameSpeed GameInProgress )

        SetPlayerName value ->
            let
                prevForm : Form
                prevForm =
                    model.form

                curForm : Form
                curForm =
                    { prevForm | name = value }
            in
            ( { model | form = curForm }, Cmd.none )

        AddToPlayerCombination value ->
            let
                prevForm : Form
                prevForm =
                    model.form

                curForm : Form
                curForm =
                    if not (List.member value prevForm.combination) && List.length prevForm.combination < 6 then
                        { prevForm | combination = value :: prevForm.combination }

                    else
                        { prevForm | combination = prevForm.combination |> List.filter (\n -> n /= value) }
            in
            ( { model | form = curForm }, Cmd.none )

        GameInProgress ->
            drawNumber model

        GameIsOver ->
            let
                upToDatePlayersCredit : List Player
                upToDatePlayersCredit =
                    let
                        listOfWinners : List Player
                        listOfWinners =
                            checkWinners model.players model.combination

                        winner : Player -> Maybe Player
                        winner p =
                            listOfWinners |> find (\x -> x.id == p.id)
                    in
                    model.players
                        |> List.map
                            (\player ->
                                case winner player of
                                    Nothing ->
                                        player

                                    Just p ->
                                        { p | credit = p.credit + player.credit }
                            )
            in
            ( { model | isPlaying = False, lastDrawn = Nothing, combination = [], players = upToDatePlayersCredit }, Cmd.none )


drawNumber : Model -> ( Model, Cmd Msg )
drawNumber model =
    if model.isPlaying then
        case model.lastDrawn of
            Nothing ->
                ( { model | lastDrawn = model.combination |> List.head }, postpone config.gameSpeed GameInProgress )

            Just jld ->
                let
                    curElemIndex : Maybe Int
                    curElemIndex =
                        model.combination |> elemIndex jld

                    combinationLength : Int
                    combinationLength =
                        model.combination |> List.length
                in
                case curElemIndex of
                    Nothing ->
                        ( model, Cmd.none )

                    Just cei ->
                        if cei < combinationLength - 1 then
                            ( { model | lastDrawn = getAt (cei + 1) model.combination }, postpone config.gameSpeed GameInProgress )

                        else
                            ( model, postpone 7000 GameIsOver )

    else
        ( model, Cmd.none )


checkWinners : List Player -> Combination -> List Player
checkWinners modelListPlayers modelCombination =
    let
        fn : Combination -> List Player -> List Player
        fn currChunk accumulator =
            let
                winners =
                    modelListPlayers
                        |> List.filter
                            (\pl ->
                                let
                                    skipExisting : Player -> Bool
                                    skipExisting p =
                                        accumulator |> List.all (\w -> w.id /= p.id)

                                    checkCombinations : Bool
                                    checkCombinations =
                                        pl.combination |> List.all (\n -> currChunk |> List.any (\c -> n == c))
                                in
                                checkCombinations && skipExisting pl
                            )
            in
            winners
                |> List.map (\p -> { p | credit = currChunk |> List.length |> odds |> (*) config.bet })
                |> List.append accumulator
    in
    List.foldl fn [] <| chunks modelCombination


getDrawn : Combination -> Maybe Int -> Bool -> Combination
getDrawn comb ld ip =
    if ip then
        case ld of
            Just jld ->
                let
                    curElemIndex =
                        comb |> elemIndex jld
                in
                case curElemIndex of
                    Just cei ->
                        comb |> List.take (cei + 1)

                    Nothing ->
                        []

            Nothing ->
                []

    else
        []


chunks : Combination -> List Combination
chunks comb =
    comb
        |> inits
        |> List.drop 6


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


isSelected : Int -> Combination -> String
isSelected n combination =
    if List.member n combination then
        "btn btn-success"

    else
        "btn btn-default"


isDisabled : Int -> Combination -> Bool
isDisabled num comb =
    List.length comb == 6 && not (List.member num comb)


isMatched : Combination -> Int -> Bool -> Maybe Int -> String
isMatched comb playerNumber ip ld =
    let
        drawn : Combination
        drawn =
            getDrawn comb ld ip
    in
    if List.member playerNumber drawn then
        "player-number animated flash"

    else
        "player-number"


playerForm : Bool -> String
playerForm ip =
    if ip then
        "hide-form"

    else
        "show-form"


drawOdds : Combination -> String
drawOdds comb =
    let
        length : Int
        length =
            List.length comb
    in
    if length < 6 || length == 35 then
        ""

    else
        odds length |> String.fromInt |> (++) "Bet x "


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-md-4" ]
                [ div
                    [ class (playerForm model.isPlaying) ]
                    [ div
                        [ style "display" "flex"
                        , style "margin-bottom" "10px"
                        ]
                        [ input
                            [ class "form-control"
                            , onInput SetPlayerName
                            , disabled model.isPlaying
                            , value model.form.name
                            , placeholder "Player name"
                            , style "margin-right" "5px"
                            ]
                            []
                        , button
                            [ class "btn btn-primary"
                            , onClick AddPlayer
                            , disabled (validateForm model.form || model.isPlaying)
                            ]
                            [ text "Add Player" ]
                        ]
                    , div
                        [ style "margin-bottom" "20px" ]
                        (List.range 1 48
                            |> List.map
                                (\n ->
                                    button
                                        [ onClick (AddToPlayerCombination n)
                                        , class (isSelected n model.form.combination)
                                        , disabled (isDisabled n model.form.combination)
                                        , style "width" "50px"
                                        , style "height" "50px"
                                        , style "border-radius" "0px"
                                        ]
                                        [ n |> String.fromInt |> text ]
                                )
                        )
                    ]
                , div
                    [ style "width" "300px"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    ]
                    (model.players
                        |> List.map
                            (\player ->
                                div [ style "margin-bottom" "5px" ]
                                    [ div
                                        [ style "display" "flex"
                                        , style "justify-content" "space-between"
                                        , style "font-weight" "bold"
                                        ]
                                        [ span [] [ text player.name ]
                                        , span [] [ player.credit |> String.fromInt |> (++) " $" |> text ]
                                        ]
                                    , div [ style "display" "flex" ]
                                        (player.combination
                                            |> List.sort
                                            |> List.map
                                                (\n ->
                                                    span
                                                        [ class (isMatched model.combination n model.isPlaying model.lastDrawn) ]
                                                        [ n |> String.fromInt |> (++) " " |> text ]
                                                )
                                        )
                                    ]
                            )
                    )
                 , div []
                    (checkWinners model.players (getDrawn model.combination model.lastDrawn model.isPlaying)
                        |> List.map
                            (\player ->
                                div [ style "margin-bottom" "5px" ]
                                    [ div
                                        [ style "display" "flex"
                                        , style "justify-content" "space-between"
                                        , style "font-weight" "bold"
                                        ]
                                        [ span [] [ text player.name ]
                                        , span [ style "color" "green" ] [ player.credit |> String.fromInt |> (++) " + $" |> text ]
                                        ]
                                    ]
                            )
                    )
                ]
            , div [ class "col-md-2 text-center" ]
                [ button
                    [ class "btn btn-success"
                    , onClick StartGame
                    , disabled (model.players |> List.length |> (==) 0 |> (||) model.isPlaying)
                    ]
                    [ text "Start Game" ]
                , div []
                    -- [ h2 [] [ text "Odds" ]
                    [ h3 [ class "animated tada infinite" ] [ drawOdds (getDrawn model.combination model.lastDrawn model.isPlaying) |> text ]
                    ]
                ]
            , div [ class "col-md-6" ]
                [ div
                    [ style "display" "flex"
                    , style "flex-wrap" "wrap"
                    , style "width" "400px"
                    ]
                    (getDrawn model.combination model.lastDrawn model.isPlaying
                        |> List.map
                            (\n ->
                                div
                                    [ class (getBall n |> (++) "animated flip ball ") ]
                                    [ div [ class "ball-inside" ]
                                        [ span [] [ n |> String.fromInt |> text ]
                                        ]
                                    ]
                            )
                    )
                ]
            ]
        ]


validateForm : Form -> Bool
validateForm form =
    form.combination
        |> List.length
        |> (==) 6
        |> (&&) (form.name /= "")
        |> not
