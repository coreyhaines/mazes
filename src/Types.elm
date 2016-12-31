module Types exposing (Room, Side(..))


type alias Room =
    { x : Int
    , y : Int
    , walls : Side
    }


type Side
    = All
    | Right
    | Top
