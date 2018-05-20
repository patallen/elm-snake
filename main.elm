import CssMain exposing (mainview)
import Html.Styled exposing (..)
import Html.Styled.Attributes  exposing (css)
import Point exposing (Point, point)
import Game exposing (..)
import Snake exposing (snake)
import GamePad exposing (..)
import Display exposing (draw)
import Time
import Keyboard

gFPS = 10

type Msg = Tick Time.Time | KeyMsg Keyboard.KeyCode


main : Program Never State Msg
main = program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : ( Game.State, Cmd Msg )
init =
    ( model , Cmd.none )


model : Game.State
model =
    { snake = snake 10 10 (point 1 0)
    , apple = point 2 2
    , events = [Right]
    , status = NewGame
    }



view : Game.State -> Html msg
view state =
  body
    [ css [ mainview 80 80 80 ]]
    [ Display.draw state ]

update : Msg -> Game.State -> (Game.State, Cmd msg)
update msg state =
  case msg of
    Tick(t) -> let res =
      state
        |> checkCollision
        |> checkBoundaries
        |> moveSnake
        |> cleanup
        |> updateDirection
      in (res, Cmd.none)
    KeyMsg code -> let res = (case (convertKey code) of
        NoOp -> state
        Pause -> state
        Quit -> state
        Shoot -> state
        evt -> Game.enqueueEvent state evt)
        in
        (res, Cmd.none)


subscriptions : Game.State -> Sub Msg
subscriptions model =
    Sub.batch
      [ Time.every (fpsMillis gFPS) Tick
      , Keyboard.downs KeyMsg
      ]
