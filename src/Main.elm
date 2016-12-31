module Main exposing (..)

import Editable exposing (Editable)
import Html exposing (Html, text, div, input, label, button)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput, onClick)
import Random
import List.Extra as List


type alias Model =
    { width : Editable Int
    , height : Editable Int
    , sides : Sides
    }


type Side
    = Both
    | Right
    | Top


type alias Sides =
    List Side


type Dimension
    = Width
    | Height


type Msg
    = SetSides Sides
    | SetDimension Dimension String
    | CommitDimensionChanges


scale =
    10


pickASide : Bool -> Side
pickASide which =
    if which then
        Right
    else
        Top


mazeWidth : Model -> Int
mazeWidth model =
    Editable.value model.width


mazeHeight : Model -> Int
mazeHeight model =
    Editable.value model.height


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { width = Editable.newEditing 50
            , height = Editable.newEditing 50
            , sides = []
            }

        generateSidesCmd =
            Random.map pickASide Random.bool
                |> Random.list ((mazeWidth initialModel) * (mazeHeight initialModel))
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

        SetDimension Width newWidth ->
            let
                updatedWidth =
                    case String.toInt newWidth of
                        Ok w ->
                            Editable.setBuffer model.width w

                        Err _ ->
                            model.width
            in
                ( { model | width = updatedWidth }
                , Cmd.none
                )

        SetDimension Height newValue ->
            let
                updatedHeight =
                    case String.toInt newValue of
                        Ok w ->
                            Editable.setBuffer model.height w

                        Err _ ->
                            model.height
            in
                ( { model | height = updatedHeight }
                , Cmd.none
                )

        CommitDimensionChanges ->
            ( { model
                | width = Editable.commitBuffer model.width
                , height = Editable.commitBuffer model.height
              }
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
        width =
            mazeWidth model

        pickSide side x y =
            if (x == width - 1) && (y == 0) then
                Both
            else if x == width - 1 then
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
    coordinates (mazeWidth model) (mazeHeight model)
        |> addSides model
        |> List.map roomView


scaledSizeInPx : Int -> String
scaledSizeInPx size =
    (toString (size * scale)) ++ "px"


mazeView : Model -> Html Msg
mazeView model =
    let
        width =
            scaledSizeInPx <| mazeWidth model

        height =
            scaledSizeInPx <| mazeHeight model
    in
        div [ style [ ( "position", "relative" ), ( "border-bottom", "1px solid black" ), ( "border-left", "1px solid black" ), ( "width", width ), ( "height", height ), ( "margin-left", "20px" ), ( "margin-top", "20px" ) ] ] <|
            roomsView model


selectionForm : Model -> Html Msg
selectionForm model =
    div []
        [ div []
            [ label [] [ text "Width" ]
            , input [ type_ "number", onInput (SetDimension Width), value <| toString <| Editable.bufferValue model.width ] []
            ]
        , div []
            [ label [] [ text "Height" ]
            , input [ type_ "number", onInput (SetDimension Height), value <| toString <| Editable.bufferValue model.height ] []
            ]
        , button [ onClick CommitDimensionChanges ] [ text "Set New Dimensions" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ mazeView model
        , selectionForm model
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
