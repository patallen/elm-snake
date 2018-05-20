module Game exposing (..)
import Snake exposing (Snake)
import Point exposing (Point, point)
import Time exposing (Time)
import GamePad exposing (Action(..))

type Status = Running | Paused | GameOver | NewGame


type alias State =
    { snake: Snake
    , apple: Point
    , events: List Action
    , status: Status
    }

step : State -> State
step state =
  state
    |> checkCollision
    |> checkBoundaries
    |> moveSnake
    |> cleanup
    |> updateDirection


fpsMillis : Int -> Time.Time
fpsMillis f = Time.millisecond * (1000 / toFloat f)

discardNoops : List Action -> List Action
discardNoops list =
  case list of
    NoOp::rest -> discardNoops rest
    l -> l


setDirection : Action -> State -> State
setDirection p state = let dir = toPoint p in
   case dir of
     Just dir -> { state | snake = updateVelocity state.snake dir }
     Nothing -> state

updateDirection : State -> State
updateDirection state = let {events} = state in
  let real = (discardNoops events) in
    case real of
     [] -> state
     x::xs -> state
       |> setQueue xs
       |> setDirection x


updateVelocity : Snake -> Point -> Snake
updateVelocity snake vel = {snake | velocity = vel }

enqueueEvent : State -> Action -> State
enqueueEvent state dir = {state | events = state.events ++ [dir]}

setQueue : List Action -> State -> State
setQueue list state = { state | events = list }

collides : Point -> Point -> Bool
collides p1 p2 = p1.x == p2.x && p1.y == p2.y

moveSnake : State -> State
moveSnake state = { state | snake =  Snake.move state.snake state.snake.velocity}

adjustSnake : Snake -> Snake
adjustSnake snake =
  case snake.body of
    x::xs ->
      { snake | body = [point (x.x % 60) (x.y % 40)] ++ xs }
    _ -> snake


checkBoundaries : State -> State
checkBoundaries state =
    {state | snake = adjustSnake state.snake }


cleanup : State -> State
cleanup state =
  let snake =  if not state.snake.eating then Snake.ungrow state.snake
    else state.snake
    in {state | snake = snake}


checkCollision : State -> State
checkCollision state =
  case state.snake.body of
    []   -> state
    [x]  -> state
    x::xs -> let isMember
      = List.member x xs in
        { state | status = if isMember then GameOver else Running }


toPoint : Action -> Maybe Point
toPoint dir = case dir of
    Up ->    Just (point 0 1)
    Down ->  Just (point 0 -1)
    Right -> Just (point 1 0)
    Left ->  Just (point -1 0)
    _ -> Nothing

