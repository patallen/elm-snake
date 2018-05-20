module Snake exposing (Snake, move, ungrow, snake, Block)

import Engine.Vector as Vec exposing (vec2, add2, Vec2i, sub2)


type alias Block = Vec2i
type alias SnakeBody = List Block


type alias Snake =
  { velocity: Vec2i
  , body: SnakeBody
  , eating: Bool
  }

move : Snake -> Snake
move snake =
 case snake.body of
   [] -> snake
   x::xs -> let ns = grow snake in
     if not snake.eating then ungrow ns else ns


grow : Snake -> Snake
grow snake = case snake.body of
  x::_ -> {snake | body = [add2 x snake.velocity] ++ snake.body}
  _ -> snake

ungrow : Snake -> Snake
ungrow snake = case List.reverse snake.body of
  _::xs -> { snake | body = List.reverse xs }
  _ -> snake

snake : Int -> Int -> Vec2i -> Snake
snake x y dir = {body = [vec2 x y], velocity = dir, eating = False}
