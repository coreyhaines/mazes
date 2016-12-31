module Main exposing (..)

import Generators exposing (Algorithm(..))
import Editable exposing (Editable)
import Types exposing (Room, Side)
import Html exposing (Html, text, div, input, label, button, span)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput, onClick)
import Random
import List.Extra as List


type alias Model =
    { width : Editable Int
    , height : Editable Int
    , seedForSideGenerator : Random.Seed
    , rooms : List Room
    , algorithm : Algorithm
    }


type Dimension
    = Width
    | Height


type Msg
    = SetSideSeed Random.Seed
    | SetDimension Dimension String
    | CommitDimensionChanges
    | ChooseAlgorithm Algorithm


scale =
    10


mazeWidth : Model -> Int
mazeWidth model =
    Editable.value model.width


mazeHeight : Model -> Int
mazeHeight model =
    Editable.value model.height


init : ( Model, Cmd Msg )
init =
    let
        initialWidth =
            5

        initialHeight =
            5

        initialModel =
            { width = Editable.newEditing initialWidth
            , height = Editable.newEditing initialHeight
            , seedForSideGenerator = Random.initialSeed 0
            , rooms = []
            , algorithm = Sidewinder
            }

        generateInitialSideSeedCmd =
            Random.int Random.minInt Random.maxInt
                |> Random.map Random.initialSeed
                |> Random.generate SetSideSeed
    in
        ( initialModel
        , generateInitialSideSeedCmd
        )


coordinates : Int -> Int -> List ( Int, Int )
coordinates width height =
    List.lift2 (,) (List.range 0 (width - 1)) (List.range 0 (height - 1))


generateRooms : Model -> List Room
generateRooms model =
    coordinates (mazeWidth model) (mazeHeight model)
        |> List.map (\( x, y ) -> { x = x, y = y, walls = Types.All })


generateMaze : Algorithm -> Model -> Model
generateMaze algorithm model =
    let
        mazeGenerator =
            Generators.getAlgorithm algorithm

        ( rooms, finalSeed ) =
            generateRooms model
                |> mazeGenerator { seed = model.seedForSideGenerator, width = mazeWidth model, height = mazeHeight model }
    in
        { model | algorithm = algorithm, rooms = rooms, seedForSideGenerator = finalSeed }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseAlgorithm algorithm ->
            let
                updatedModel =
                    generateMaze algorithm model
            in
                ( updatedModel
                , Cmd.none
                )

        SetSideSeed seed ->
            let
                updatedModel =
                    generateMaze model.algorithm { model | seedForSideGenerator = seed }
            in
                ( updatedModel
                , Cmd.none
                )

        CommitDimensionChanges ->
            let
                updatedModel =
                    generateMaze model.algorithm { model | width = Editable.commitBuffer model.width, height = Editable.commitBuffer model.height }
            in
                ( updatedModel
                , Cmd.none
                )

        SetDimension which newValue ->
            let
                updateProperty property =
                    case String.toInt newValue of
                        Ok w ->
                            Editable.setBuffer property w

                        Err _ ->
                            property

                updatedModel =
                    case which of
                        Height ->
                            { model | height = updateProperty model.height }

                        Width ->
                            { model | width = updateProperty model.width }
            in
                ( updatedModel
                , Cmd.none
                )


roomView : Room -> Html Msg
roomView { x, y, walls } =
    let
        left =
            scaledSizeInPx x

        top =
            scaledSizeInPx y

        width =
            scaledSizeInPx 1

        ( topBorder, rightBorder ) =
            case walls of
                Types.Right ->
                    ( "0px", "1px solid black" )

                Types.Top ->
                    ( "1px solid black", "0px" )

                Types.All ->
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


roomsView : Model -> List (Html Msg)
roomsView model =
    List.map roomView model.rooms


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
    let
        algorithmChooser algorithm currentAlgorithm label =
            if algorithm == currentAlgorithm then
                span [] [ text <| "<X> " ++ label ]
            else
                span [ onClick <| ChooseAlgorithm algorithm ] [ text <| "< > " ++ label ]
    in
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
            , div []
                [ text "Algorithm"
                , span [] [ text " | " ]
                , algorithmChooser PlainGrid model.algorithm "None"
                , span [] [ text " | " ]
                , algorithmChooser BinaryTree model.algorithm "Binary Tree"
                , span [] [ text " | " ]
                , algorithmChooser Sidewinder model.algorithm "Sidewinder"
                ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ selectionForm model
        , mazeView model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
