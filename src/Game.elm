module Game
    exposing
        ( GameState
        , Msg(..)
        , defaultGameState
        , enqueueEvent
        , fpsMillis
        , step
        )

import Engine.Environment exposing (Environment, environ)
import Engine.Vector exposing (vec2)
import GamePad exposing (Action(..))
import Keyboard
import Random exposing (Generator)
import Snake exposing (Block, Snake, snake)
import Time exposing (Time)


type Msg
    = Tick Time.Time
    | KeyMsg Keyboard.KeyCode
    | RandomApple ( Int, Int )


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


defaultGameState : Int -> Int -> GameState
defaultGameState width height =
    { snake = snake 10 10 ( 1, 0 )
    , environ = environ width height
    , apple = ( 9, 10 )
    , events = [ Up ]
    , status = NewGame
    }


checkGameOver : GameState -> GameState
checkGameOver state =
    if state.status == GameOver then
        { state | snake = snake 10 10 ( 1, 0 ) }
    else
        state


step : GameState -> ( GameState, Cmd Msg )
step state =
    let
        newState =
            state
                |> checkCollision
                |> moveSnake
                |> checkFood
                |> wrapBoundaries
                |> updateDirection
                |> checkGameOver
    in
    ( newState
    , if newState.snake.eating then
        let
            ( _, ( x, y ) ) =
                state.environ.bounds
        in
        generateApple x y
      else
        Cmd.none
    )


checkFood : GameState -> GameState
checkFood state =
    state
        |> (\{ snake } ->
                let
                    sn =
                        { snake | eating = Snake.willEat snake state.apple }
                in
                { state | snake = sn }
           )


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


adjustSnake : Snake -> ( Int, Int ) -> Snake
adjustSnake snake ( x, y ) =
    case snake.body of
        ( lx, ly ) :: xs ->
            { snake | body = [ vec2 (lx % x) (ly % y) ] ++ xs }

        _ ->
            snake


wrapBoundaries : GameState -> GameState
wrapBoundaries state =
    { state
        | snake =
            adjustSnake state.snake <|
                Tuple.second state.environ.bounds
    }


checkCollision : GameState -> GameState
checkCollision state =
    case state.snake.body of
        [] ->
            state

        [ x ] ->
            state

        x :: xs ->
            { state
                | status =
                    if List.member x xs then
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


pairGenerator : Int -> Int -> Generator ( Int, Int )
pairGenerator x y =
    Random.pair (Random.int 0 x) (Random.int 0 y)


generateApple : Int -> Int -> Cmd Msg
generateApple x y =
    Random.generate RandomApple (pairGenerator x y)
