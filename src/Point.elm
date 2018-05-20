module Point exposing (Point, point, add, default)

type alias Point = {x: Int , y: Int}

add : Point -> Point -> Point
add p1 p2 = {x = p1.x + p2.x, y = p1.y + p2.y}

point : Int -> Int -> Point
point x y = {x = x, y = y}

default : Int -> Int -> Maybe Point -> Point
default x y mby = Maybe.withDefault (point x y) mby
