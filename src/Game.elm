module Game exposing (GameState, defaultGameState, enqueueEvent, fpsMillis, step)

import Engine.Environment exposing (Environment, environ)
import Engine.Vector exposing (vec2)
import GamePad exposing (Action(..))
import Snake exposing (Block, Snake, snake)
import Time exposing (Time)


type Status
    = Running
    | Paused
    | GameOver
    | NewGame


type alias GameState =
    { snake : Snake
    , environ : Environment
    , apple : Block
    , events : List Action
    , status : Status
    }


defaultGameState : GameState
defaultGameState =
    { snake = snake 10 10 ( 1, 0 )
    , environ = environ 60 40 20
    , apple = ( 9, 10 )
    , events = [ Up ]
    , status = NewGame
    }


checkGameOver : GameState -> GameState
checkGameOver state =
    if state.status == GameOver then
        defaultGameState
    else
        state


step : GameState -> GameState
step state =
    state
        |> checkGameOver
        |> checkCollision
        |> moveSnake
        |> wrapBoundaries
        |> updateDirection


fpsMillis : Int -> Time.Time
fpsMillis f =
    Time.millisecond * (1000 / toFloat f)


discardNoops : List Action -> List Action
discardNoops list =
    case list of
        NoOp :: rest ->
            discardNoops rest

        l ->
            l


setDirection : Action -> GameState -> GameState
setDirection p state =
    let
        dir =
            toPoint p
    in
    case dir of
        Just dir ->
            { state | snake = updateVelocity state.snake dir }

        Nothing ->
            state


updateDirection : GameState -> GameState
updateDirection state =
    let
        { events } =
            state
    in
    let
        real =
            discardNoops events
    in
    case real of
        [] ->
            state

        x :: xs ->
            state
                |> setQueue xs
                |> setDirection x


updateVelocity : Snake -> Block -> Snake
updateVelocity snake vel =
    { snake | velocity = vel }


enqueueEvent : GameState -> Action -> GameState
enqueueEvent state dir =
    { state | events = state.events ++ [ dir ] }


setQueue : List Action -> GameState -> GameState
setQueue list state =
    { state | events = list }


moveSnake : GameState -> GameState
moveSnake state =
    { state | snake = Snake.move state.snake }


adjustSnake : Snake -> Snake
adjustSnake snake =
    case snake.body of
        ( lx, ly ) :: xs ->
            { snake | body = [ vec2 (lx % 60) (ly % 40) ] ++ xs }

        _ ->
            snake


wrapBoundaries : GameState -> GameState
wrapBoundaries state =
    { state | snake = adjustSnake state.snake }


checkCollision : GameState -> GameState
checkCollision state =
    case state.snake.body of
        [] ->
            state

        [ x ] ->
            state

        x :: xs ->
            let
                isMember =
                    List.member x xs
            in
            { state
                | status =
                    if isMember then
                        GameOver
                    else
                        Running
            }


toPoint : Action -> Maybe Block
toPoint dir =
    case dir of
        Up ->
            Just (vec2 0 1)

        Down ->
            Just (vec2 0 -1)

        Right ->
            Just (vec2 1 0)

        Left ->
            Just (vec2 -1 0)

        _ ->
            Nothing
