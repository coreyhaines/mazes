module Main exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Random


type alias Model =
    { size : Int
    , sides : Sides
    }


type Side
    = Right
    | Top


type alias Sides =
    List Side


type Msg
    = SetSides Sides


scale =
    3


pickASide : Bool -> Side
pickASide which =
    if which then
        Right
    else
        Top


init : ( Model, Cmd Msg )
init =
    let
        size =
            10
    in
        ( { size = size, sides = [] }
        , Random.generate SetSides (Random.list (size * size) <| Random.map pickASide Random.bool)
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


borderView : Int -> Html Msg
borderView size =
    let
        pixels =
            (toString (size * size * scale)) ++ "px"
    in
        div [ style [ ( "position", "relative" ), ( "border", "1px solid black" ), ( "width", pixels ), ( "height", pixels ) ] ]
            []


view : Model -> Html Msg
view model =
    div []
        [ borderView model.size
        , Html.hr [] []
        , text <| toString model
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
