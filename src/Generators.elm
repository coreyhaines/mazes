module Generators exposing (Algorithm(..), getAlgorithm)

import Random
import Types exposing (Room, Side(..))
import List.Extra as List


type Algorithm
    = PlainGrid
    | BinaryTree
    | Sidewinder


type alias MazeGenerator =
    { seed : Random.Seed, width : Int, height : Int } -> List Room -> ( List Room, Random.Seed )


plainGridAlgorithm : MazeGenerator
plainGridAlgorithm { seed } rooms =
    ( rooms, seed )


binaryTreeAlgorithm : MazeGenerator
binaryTreeAlgorithm { seed, width } rooms =
    let
        pickASide which =
            if which then
                Types.Right
            else
                Types.Top

        nextWalls fromSeed room =
            if (room.y == 0) && (room.x == width - 1) then
                ( Types.All, fromSeed )
            else if room.y == 0 then
                ( Types.Top, fromSeed )
            else if room.x == width - 1 then
                ( Types.Right, fromSeed )
            else
                Random.step (Random.map pickASide Random.bool) fromSeed

        figureOutSide room ( updatedRooms, currentSeed ) =
            let
                ( walls, nextSeed ) =
                    nextWalls currentSeed room
            in
                ( { room | walls = walls } :: updatedRooms
                , nextSeed
                )
    in
        List.foldl
            figureOutSide
            ( [], seed )
            rooms


sidewinderAlgorithm : MazeGenerator
sidewinderAlgorithm { seed, width, height } rooms =
    let
        getRow index rooms =
            List.filter (\{ x, y, walls } -> y == index) rooms

        sameRoom room1 room2 =
            (room1.x == room2.x && room1.y == room2.y)

        replaceRoom room ( updatedRooms, currentSeed ) =
            ( List.replaceIf (sameRoom room) room updatedRooms
            , currentSeed
            )

        replaceRooms index rooms ( roomsToReplace, seed ) =
            List.foldl
                replaceRoom
                ( rooms, seed )
                roomsToReplace

        updateTopRow width index ( rooms, seed ) =
            ( List.map
                (\room ->
                    if room.x < width - 1 then
                        { room | walls = Top }
                    else
                        { room | walls = All }
                )
                rooms
            , seed
            )

        processRow width height index ( rooms, seed ) =
            ( rooms, seed )

        updateRow processor index ( rooms, seed ) =
            ( getRow index rooms, seed )
                |> processor index
                |> replaceRooms index rooms
    in
        updateRow (updateTopRow width) 0 ( rooms, seed )
            |> updateRow (processRow width height) 1


getAlgorithm : Algorithm -> MazeGenerator
getAlgorithm algorithm =
    case algorithm of
        PlainGrid ->
            plainGridAlgorithm

        BinaryTree ->
            binaryTreeAlgorithm

        Sidewinder ->
            sidewinderAlgorithm
