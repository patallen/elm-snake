module GamePad exposing (..)

import Keyboard exposing (KeyCode)


type Action
    = Up
    | Down
    | Left
    | Right
    | Shoot
    | Pause
    | Quit
    | NoOp


convertKey : KeyCode -> Action
convertKey key =
    case key of
        87 ->
            Up

        83 ->
            Down

        65 ->
            Left

        68 ->
            Right

        32 ->
            Shoot

        80 ->
            Pause

        27 ->
            Quit

        _ ->
            NoOp
