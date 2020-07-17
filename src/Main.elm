module Main exposing (..)

import Array exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import List.Extra exposing (elemIndex, getAt, inits, removeAt)
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
    { name : String
    , combination : Combination
    }



-- MODEL


type alias Model =
    { isPlaying : Bool
    , lastDrawn : Maybe Int
    , players : List Player
    , combination : Combination
    , form : Player
    }


initialForm : Player
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


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate GameCombination (Random.int 1 48)


delay : Float -> Cmd Msg
delay time =
    Process.sleep time
        |> Task.perform (\_ -> GameInProgress)



-- UPDATE


type Msg
    = StartGame
    | GameCombination Int
    | AddPlayer
    | SetPlayerName String
    | AddToPlayerCombination Int
    | GameInProgress
    | GameIsOver


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlayer ->
            ( { model
                | players =
                    model.form :: model.players
                , form = initialForm
              }
            , Cmd.none
            )

        StartGame ->
            ( model, generateRandomNumber )

        GameCombination number ->
            if List.length model.combination < 35 then
                if List.member number model.combination then
                    ( model, generateRandomNumber )

                else
                    ( { model | combination = number :: model.combination }, generateRandomNumber )

            else
                ( { model | combination = model.combination, isPlaying = True }, delay 1000 )

        SetPlayerName value ->
            let
                prevForm =
                    model.form

                curForm =
                    { prevForm | name = value }
            in
            ( { model | form = curForm }, Cmd.none )

        AddToPlayerCombination value ->
            let
                prevForm =
                    model.form

                curForm =
                    if not (List.member value prevForm.combination) && List.length prevForm.combination < 6 then
                        { prevForm | combination = value :: prevForm.combination }

                    else
                        { prevForm
                            | combination =
                                prevForm.combination
                                    |> List.filter (\n -> n /= value)
                        }
            in
            ( { model | form = curForm }, Cmd.none )

        GameInProgress ->
            drawNumber model

        GameIsOver ->
            ( model, Cmd.none )


drawNumber : Model -> ( Model, Cmd Msg )
drawNumber model =
    if model.isPlaying then
        case model.lastDrawn of
            Nothing ->
                ( { model | lastDrawn = model.combination |> List.head }, delay 1000 )

            Just jld ->
                let
                    curElemIndex =
                        model.combination |> elemIndex jld

                    combinationLen =
                        model.combination |> List.length
                in
                case curElemIndex of
                    Nothing ->
                        ( model, Cmd.none )

                    Just cei ->
                        if cei < combinationLen - 1 then
                            ( { model | lastDrawn = getAt (cei + 1) model.combination }, delay 1000 )

                        else
                            -- https://medium.com/elm-shorts/how-to-turn-a-msg-into-a-cmd-msg-in-elm-5dd095175d84
                            { model | isPlaying = False, lastDrawn = Nothing } |> update GameIsOver

    else
        ( model, Cmd.none )



-- VIEW


selected : Int -> Combination -> String
selected n combination =
    if List.member n combination then
        "red"

    else
        ""


blink : Int -> Maybe Int -> String
blink n ld =
    case ld of
        Nothing ->
            ""

        Just i ->
            if i == n then
                "red"

            else
                ""


foo : Maybe Int -> String
foo a =
    case a of
        Nothing ->
            ""

        Just s ->
            String.fromInt s


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
        |> removeAt 5


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text
                (if model.isPlaying then
                    "Game Is Live"

                 else
                    "Game Is IDLE"
                )
            , span [] [ text (foo model.lastDrawn) ]
            ]
        , button [ onClick StartGame ] [ text "Start Game" ]
        , button [ onClick AddPlayer, disabled (validateForm model.form) ] [ text "Add Player" ]
        , input [ onInput SetPlayerName, value model.form.name ] []
        , div []
            (model.players
                |> List.map
                    (\player ->
                        div []
                            [ text player.name
                            , div []
                                (player.combination
                                    |> List.map
                                        (\n ->
                                            span
                                                [ style "color"
                                                    (if List.member n (getDrawn model.combination model.lastDrawn model.isPlaying) then
                                                        "red"

                                                     else
                                                        ""
                                                    )
                                                ]
                                                [ text (String.fromInt n ++ " ,") ]
                                        )
                                )
                            ]
                    )
            )
        , div [ style "width" "200px", style "display" "flex", style "flex-wrap" "wrap" ]
            (List.range 1 48
                |> List.map
                    (\n ->
                        button
                            [ onClick (AddToPlayerCombination n)
                            , style "width" "30px"
                            , style "height" "30px"
                            , style "background-color" (selected n model.form.combination)
                            ]
                            [ text (String.fromInt n) ]
                    )
            )
        , div []
            (getDrawn model.combination model.lastDrawn model.isPlaying
                |> List.map
                    (\n ->
                        div []
                            [ text (String.fromInt n) ]
                    )
            )
        ]


validateForm : Player -> Bool
validateForm form =
    not (List.length form.combination == 6 && form.name /= "")
