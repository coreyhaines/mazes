module Binary where
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)

roomSize : Int
roomSize = 30

gridSize : Int
gridSize = 10

type alias Room =
  {
    x: Int
  , y: Int
  , northWall: Bool
  , eastWall: Bool
  }

type alias Maze = List Room


makeGrid : Int -> Int -> Maze
makeGrid width height =
  [0..width-1]
  |> List.map Room
  |> List.concatMap (\room -> (List.map room [0..height-1]))
  |> List.map (\room -> room True True)

maze : Maze
maze = makeGrid gridSize gridSize


northWall : Room -> List (Float, Float)
northWall room =
  let floatSize = toFloat roomSize
  in
    case room.northWall of
      True ->
        [(0, floatSize), (floatSize, floatSize)]
      _ ->
        []

eastWall : Room -> List (Float, Float)
eastWall room =
  let floatSize = toFloat roomSize
  in
    case room.eastWall of
      True ->
        [(floatSize, floatSize), (floatSize, 0)]
      _ ->
        []

viewRoom : Room -> Form
viewRoom room =
  (northWall room) ++ (eastWall room)
  |> path
  |> traced (solid black)

roomPosition : Room -> (Float, Float)
roomPosition room =
  ((toFloat (room.x*roomSize)), (toFloat (room.y*roomSize)))

showRoom : Room -> Form
showRoom room =
  viewRoom room
  |> move (roomPosition room)

leftBottomWalls : Int -> Form
leftBottomWalls size =
  let corner = (toFloat (size*roomSize))
  in [(0, corner), (0,0), (corner, 0)]
  |> path
  |> traced (solid black)

viewMaze : Maze -> List Form
viewMaze maze =
  (leftBottomWalls gridSize) :: (List.map showRoom maze)

main : Element
main =
  collage 1000 1000 (viewMaze maze)
