module Main exposing (..)

import Display exposing (Pixel, draw, pixel)
import Game exposing (..)
import GamePad exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Keyboard
import Styles.CssMain exposing (mainview)
import Time


gWIDTH : Int
gWIDTH =
    40


gHEIGHT : Int
gHEIGHT =
    30


gBLOCKSIZE : Int
gBLOCKSIZE =
    20


gGREEN : ( Int, Int, Int )
gGREEN =
    ( 0, 200, 0 )


gRED : ( Int, Int, Int )
gRED =
    ( 200, 0, 0 )


gFPS : Int
gFPS =
    20


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
    defaultGameState gWIDTH gHEIGHT


view : GameState -> Html msg
view state =
    let
        { snake, apple } =
            state
    in
    body
        [ css [ mainview 80 80 80 ] ]
        [ draw 40 30 20 <|
            List.map (pixel gGREEN) snake.body
                ++ [ pixel gRED apple ]
        ]


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg state =
    case msg of
        RandomApple ( x, y ) ->
            ( { state | apple = ( x, y ) }, Cmd.none )

        Tick t ->
            step state

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
