module Main exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Random
import List.Extra as List


type alias Model =
    { width : Int
    , height : Int
    , sides : Sides
    }


type Side
    = Both
    | Right
    | Top


type alias Sides =
    List Side


type Msg
    = SetSides Sides


scale =
    15


size =
    20


pickASide : Bool -> Side
pickASide which =
    if which then
        Right
    else
        Top


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { width = size
            , height = size
            , sides = []
            }

        generateSidesCmd =
            Random.map pickASide Random.bool
                |> Random.list (initialModel.width * initialModel.height)
                |> Random.generate SetSides
    in
        ( initialModel
        , generateSidesCmd
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


roomView : ( Int, Int, Side ) -> Html Msg
roomView ( x, y, side ) =
    let
        left =
            scaledSizeInPx x

        top =
            scaledSizeInPx y

        width =
            scaledSizeInPx 1

        ( topBorder, rightBorder ) =
            case side of
                Right ->
                    ( "0px", "1px solid black" )

                Top ->
                    ( "1px solid black", "0px" )

                Both ->
                    ( "1px solid black", "1px solid black" )
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "border-top", topBorder )
                , ( "border-right", rightBorder )
                , ( "padding", "0" )
                , ( "margin", "0" )
                , ( "left", left )
                , ( "top", top )
                , ( "width", width )
                , ( "height", width )
                ]
            ]
            []


coordinates : Int -> Int -> List ( Int, Int )
coordinates width height =
    List.lift2 (,) (List.range 0 (width - 1)) (List.range 0 (height - 1))


addSides : Model -> List ( Int, Int ) -> List ( Int, Int, Side )
addSides model coordinates =
    let
        pickSide side x y =
            if (x == model.width - 1) && (y == 0) then
                Both
            else if x == model.width - 1 then
                Right
            else if y == 0 then
                Top
            else
                side

        addSide side ( x, y ) =
            ( x, y, pickSide side x y )
    in
        List.map2 addSide model.sides coordinates


roomsView : Model -> List (Html Msg)
roomsView model =
    coordinates model.width model.height
        |> addSides model
        |> List.map roomView


scaledSizeInPx : Int -> String
scaledSizeInPx size =
    (toString (size * scale)) ++ "px"


mazeView : Model -> Html Msg
mazeView model =
    let
        width =
            scaledSizeInPx model.width

        height =
            scaledSizeInPx model.height
    in
        div [ style [ ( "position", "relative" ), ( "border", "1px solid black" ), ( "width", width ), ( "height", height ) ] ] <|
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
