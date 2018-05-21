module Display exposing (Pixel, draw, pixel)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Styles.CssMain exposing (game, pixelCss)


type alias RGB =
    ( Int, Int, Int )


type alias Coord =
    ( Int, Int )


type alias Pixel =
    { loc : Coord
    , color : RGB
    }


pixel : RGB -> Coord -> Pixel
pixel color loc =
    { loc = loc, color = color }


drawPixel : Int -> Pixel -> Html msg
drawPixel size { loc, color } =
    div [ css [ pixelCss size color loc ] ] []


drawPixels : Int -> List Pixel -> List (Html msg)
drawPixels size pixels =
    List.map (drawPixel size) pixels


draw : Int -> Int -> Int -> List Pixel -> Html msg
draw w h bs pixels =
    div [ css [ game w h bs ] ] (drawPixels bs pixels)
