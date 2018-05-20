module Main exposing (..)

import Display exposing (Pixel, draw, pixel)
import Game exposing (..)
import GamePad exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Keyboard
import Styles.CssMain exposing (mainview)
import Time


gFPS =
    10


gWIDTH =
    60


gHEIGHT =
    40


gBLOCKSIZE =
    20


type Msg
    = Tick Time.Time
    | KeyMsg Keyboard.KeyCode


main : Program Never GameState Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( GameState, Cmd Msg )
init =
    ( model, Cmd.none )


model : GameState
model =
    defaultGameState


view : GameState -> Html msg
view state =
    body
        [ css [ mainview 80 80 80 ] ]
        [ Display.draw (List.map (pixel ( 0, 200, 0 )) state.snake.body ++ [ pixel ( 200, 0, 0 ) state.apple ]) ]


update : Msg -> GameState -> ( GameState, Cmd msg )
update msg state =
    case msg of
        Tick t ->
            ( step state, Cmd.none )

        KeyMsg code ->
            let
                res =
                    case convertKey code of
                        NoOp ->
                            state

                        Pause ->
                            state

                        Quit ->
                            state

                        Shoot ->
                            state

                        evt ->
                            Game.enqueueEvent state evt
            in
            ( res, Cmd.none )


subscriptions : GameState -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (fpsMillis gFPS) Tick
        , Keyboard.downs KeyMsg
        ]
