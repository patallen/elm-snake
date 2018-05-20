module Snake exposing (Snake, move, head, ungrow, snake)

import Point exposing (Point, point)

type alias Snake =
  { velocity: Point
  , body: List Point
  , eating: Bool
  }

move : Snake -> Point -> Snake
move snake dir =
  { snake
  | body =
    List.append snake.body [Point.add dir (head snake)] }

head : Snake -> Point
head snake = Maybe.withDefault (point 0 0) (List.head (List.reverse snake.body))


ungrow : Snake -> Snake
ungrow snake = case snake.body of
  _::xs -> { snake | body = xs }
  _ -> snake

snake : Int -> Int -> Point -> Snake
snake x y dir = {body = [point x y], velocity = dir, eating = False}
