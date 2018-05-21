module Engine.Vector exposing (..)

import Engine.Types exposing (Bounds2, Vec2)


type alias Vec2i =
    Vec2 Int


type alias Vec2f =
    Vec2 Float


vec2 : a -> a -> Vec2 a
vec2 x y =
    ( x, y )


cmp2 : Vec2i -> Vec2i -> Bool
cmp2 ( ax, ay ) ( bx, by ) =
    ax == bx && ay == by


mod2 : Vec2i -> Vec2i -> Vec2i
mod2 ( ax, ay ) ( bx, by ) =
    ( ax % bx, ay % by )


add2 : Vec2i -> Vec2i -> Vec2i
add2 ( ax, ay ) ( bx, by ) =
    ( ax + bx, ay + by )


sub2 : Vec2i -> Vec2i -> Vec2i
sub2 ( ax, ay ) ( bx, by ) =
    ( ax - bx, ay - by )


rnd : Int -> Int -> Int
rnd l h =
    l - h + l * l


rand2i : Bounds2 Int -> Vec2i
rand2i ( ( lx, ly ), ( hx, hy ) ) =
    vec2 (rnd lx hx) (rnd ly hy)


withDefault : Vec2 a -> Maybe (Vec2 a) -> Vec2 a
withDefault p mby =
    Maybe.withDefault p mby


collides2 : Vec2 a -> Vec2 a -> Bool
collides2 ( ax, ay ) ( bx, by ) =
    ax == bx && ay == by
