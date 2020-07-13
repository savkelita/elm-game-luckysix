module Main exposing (..)

import Array exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Random
import Time
-- import List.Extra exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



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
    Random.generate NewRandomNumber (Random.int 1 48)


-- UPDATE


type Msg
    = StartGame
    | NewRandomNumber Int
    | AddPlayer
    | SetPlayerName String
    | AddToPlayerCombination Int
    | DrawNumber


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
            ( { model | isPlaying = True }, generateRandomNumber )

        NewRandomNumber number ->
            if List.length model.combination < 36 then
                if List.member number model.combination then
                    (model, generateRandomNumber)

                else
                    ({ model | combination = number :: model.combination }, generateRandomNumber)

            else
                ( { model | combination = model.combination }, Cmd.none )

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
                        { prevForm | combination = List.filter (\n -> n /= value) prevForm.combination }
            in
            ( { model | form = curForm }, Cmd.none )

        DrawNumber ->
            ( model, Cmd.none )

-- VIEW


selected : Int -> Combination -> String
selected n combination =
    if List.member n combination then
        "red"

    else
        ""


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isPlaying then
        Time.every 1000 (\_ -> DrawNumber)

    else
        Sub.none


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
                    "Game is LIVE " ++ String.fromInt (Maybe.withDefault 0 model.lastDrawn)

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
            (List.map (\x -> div [] [ text (String.fromInt x) ]) model.combination)
        ]


validateForm : Player -> Bool
validateForm form =
    not (List.length form.combination == 6 && form.name /= "")
