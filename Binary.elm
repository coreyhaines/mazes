module Binary where
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)

roomSize : Int
roomSize = 30

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
maze = makeGrid 10 10

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
  traced (solid black) (path ((northWall room) ++ (eastWall room)))

roomPosition : Room -> (Float, Float)
roomPosition room =
  ((toFloat (room.x*roomSize)), (toFloat (room.y*roomSize)))


showRoom : Room -> Form
showRoom room =
  viewRoom room
  |> move (roomPosition room)

main : Element
main =
  collage 1000 1000 (List.map showRoom maze)
