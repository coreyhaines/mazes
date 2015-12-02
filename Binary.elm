module Binary where
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)




roomSize : Int
roomSize = 10

type alias Room =
  {
    x: Int
  , y: Int
  }

type alias Maze = List Room


makeGrid : Int -> Int -> Maze
makeGrid width height =
  [0..width-1]
  |> List.map Room
  |> List.map (\room -> (List.map room [0..height-1]))
  |> List.concat

maze : Maze
maze = makeGrid 5 5

viewRoom : Room -> Form
viewRoom room =
  outlined (solid black) (square (toFloat roomSize))

roomPosition : Room -> (Float, Float)
roomPosition room =
  ((toFloat (room.x*roomSize)), (toFloat (room.y*roomSize)))


showRoom : Room -> Form
showRoom room =
  viewRoom room
  |> move (roomPosition room)

main : Element
main =
  collage 200 200 (List.map showRoom maze)
