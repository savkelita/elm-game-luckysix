module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Combination


type alias Combination =
    List Int



-- Player


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


init : Model
init =
    { isPlaying = False
    , lastDrawn = Nothing
    , players = []
    , combination = []
    , form = initialForm
    }



-- UPDATE


type Msg
    = AddPlayer
    | StartGame
    | SetPlayerName String
    | AddToPlayerCombination Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPlayer ->
            { model
                | players =
                    model.form :: model.players
                , form = initialForm
            }

        StartGame ->
            { model | isPlaying = not model.isPlaying }

        SetPlayerName value ->
            let
                prevForm =
                    model.form

                curForm =
                    { prevForm | name = value }
            in
            { model | form = curForm }

        AddToPlayerCombination value ->
            let
                prevForm =
                    model.form

                curForm =
                    if not (List.member value prevForm.combination) && List.length prevForm.combination < 6 then
                        { prevForm | combination = value :: prevForm.combination }

                    else
                        { prevForm | combination = List.filter (\n -> n /= value) prevForm.combination }
            in
            { model | form = curForm }



-- VIEW


selected : Int -> Combination -> String
selected n combination =
    if List.member n combination then
        "red"

    else
        ""


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick StartGame ] [ text "Start Game" ]
        , button [ onClick AddPlayer, disabled (validateForm model.form) ] [ text "Add Player" ]
        , input [ onInput SetPlayerName, value model.form.name ] []
        , div [] (List.map (\n -> div [] [ text n.name ]) model.players)
        , div []
            [ text
                (if model.isPlaying then
                    "Game is LIVE"

                 else
                    "Game is IDLE"
                )
            ]
        , div []
            [ text model.form.name ]
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
            (List.map (\x -> div [] [ text (String.fromInt x) ]) model.form.combination)
        ]


validateForm : Player -> Bool
validateForm form =
    not (List.length form.combination == 6 && form.name /= "")
