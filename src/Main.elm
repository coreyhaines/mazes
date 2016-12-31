module Main exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Random
import List.Extra as List


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
    15


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


roomView : ( Int, Int ) -> Html Msg
roomView ( x, y ) =
    let
        left =
            (toString (x * scale)) ++ "px"

        top =
            (toString (y * scale)) ++ "px"

        width =
            (toString scale) ++ "px"
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "border", "1px solid black" )
                , ( "padding", "0" )
                , ( "margin", "0" )
                , ( "left", left )
                , ( "top", top )
                , ( "width", width )
                , ( "height", width )
                ]
            ]
            []


roomsView : Model -> List (Html Msg)
roomsView model =
    let
        range =
            List.range 0 (model.size - 1)

        coordinates =
            List.lift2 (,) range range
    in
        List.map roomView coordinates


mazeView : Model -> Html Msg
mazeView model =
    let
        pixels =
            (toString (model.size * scale)) ++ "px"
    in
        div [ style [ ( "position", "relative" ), ( "border", "1px solid black" ), ( "width", pixels ), ( "height", pixels ) ] ] <|
            roomsView model


view : Model -> Html Msg
view model =
    div []
        [ mazeView model
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
