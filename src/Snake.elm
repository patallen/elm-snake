module Snake exposing (Block, Snake, move, snake, ungrow, willEat)

import Engine.Vector as Vec exposing (Vec2i, add2, cmp2, sub2, vec2)


type alias Block =
    Vec2i


type alias SnakeBody =
    List Block


type alias Snake =
    { velocity : Vec2i
    , body : SnakeBody
    , eating : Bool
    }


move : Snake -> Snake
move snake =
    case snake.body of
        [] ->
            snake

        x :: xs ->
            let
                ns =
                    grow snake
            in
            if not snake.eating then
                ungrow ns
            else
                ns


willEat : Snake -> Vec2i -> Bool
willEat { body } apple =
    case body of
        x :: _ ->
            cmp2 x apple

        _ ->
            False


grow : Snake -> Snake
grow snake =
    case snake.body of
        x :: _ ->
            { snake | body = [ add2 x snake.velocity ] ++ snake.body }

        _ ->
            snake


ungrow : Snake -> Snake
ungrow snake =
    case List.reverse snake.body of
        [ x ] ->
            { snake | body = [ x ] }

        _ :: xs ->
            { snake | body = List.reverse xs }

        _ ->
            snake


snake : Int -> Int -> Vec2i -> Snake
snake x y dir =
    { body = [ vec2 x y ], velocity = dir, eating = False }
