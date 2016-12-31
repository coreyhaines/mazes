module Main exposing (..)

import Html exposing (Html, text)
import Random


type alias Model =
    { sides : Sides
    }


type Side
    = Right
    | Top


type alias Sides =
    List Side


type Msg
    = SetSides Sides


pickASide : Bool -> Side
pickASide which =
    if which then
        Right
    else
        Top


init : ( Model, Cmd Msg )
init =
    ( { sides = [] }
    , Random.generate SetSides (Random.list 5 <| Random.map pickASide Random.bool)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSides sides ->
            ( { model | sides = sides }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    text <| toString model


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
