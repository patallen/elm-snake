module Styles.CssMain exposing (game, mainview, pixelCss)

import Css
    exposing
        ( Style
        , absolute
        , alignItems
        , backgroundColor
        , bottom
        , center
        , display
        , displayFlex
        , height
        , hidden
        , inlineBlock
        , justifyContent
        , left
        , overflow
        , pct
        , position
        , property
        , px
        , relative
        , rgb
        , top
        , width
        )


game : Int -> Int -> Int -> Style
game w h blocksize =
    Css.batch
        [ width <| px (toFloat (w * blocksize))
        , height <| px (toFloat (h * blocksize))
        , position relative
        , overflow hidden
        , backgroundColor (rgb 20 20 20)
        ]


mainview : Int -> Int -> Int -> Style
mainview r g b =
    Css.batch
        [ displayFlex
        , alignItems center
        , justifyContent center
        , backgroundColor (rgb r g b)
        , width (pct 100)
        , height (pct 100)
        ]


pixelCss : Int -> ( Int, Int, Int ) -> ( Int, Int ) -> Style
pixelCss size ( r, g, b ) ( x, y ) =
    Css.batch
        [ display inlineBlock
        , position absolute
        , width (px (toFloat size))
        , height (px (toFloat size))
        , backgroundColor (rgb r g b)
        , bottom (px (toFloat (size * y)))
        , left (px (toFloat (size * x)))
        ]
