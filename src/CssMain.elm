module CssMain exposing (..)

import Css exposing (..)


blocksize : Int
blocksize = 20


block : String -> Int ->  {x: Int, y: Int} -> Style
block color size pos = Css.batch
        [ width (px (toFloat size))
        , height (px (toFloat size))
        , property "background" color
        , display inlineBlock
        , position absolute
        , top (px (toFloat (pos.y * size)))
        , left (px (toFloat (pos.x * size)))
        ]


game : Int -> Int -> Int -> Style
game w h blocksize  = Css.batch
        [ width <| px (toFloat (w * blocksize))
        , height <| px (toFloat (h * blocksize))
        , position relative
        , overflow hidden
        , backgroundColor (rgb 0 0 0)
        ]


mainview : Int ->  Int -> Int -> Style
mainview r g b =
  Css.batch
    [ displayFlex
    , alignItems center
    , justifyContent center
    , backgroundColor (rgb r g b)
    , width (pct 100)
    , height (pct 100)
    ]

snakeBlock : {x: Int, y: Int} -> Style
snakeBlock position = block "green" blocksize position


apple : {x: Int, y: Int} -> Style
apple position = block "red" blocksize position
