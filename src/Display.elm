module Display exposing (draw)
import Css exposing (Style, displayFlex, alignItems, center, justifyContent)
import Point exposing (Point)
import CssMain exposing (snakeBlock, apple, game)
import Game exposing (State)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Snake exposing (Snake)


drawBlock : (Point -> Style)  -> Point -> Html msg
drawBlock blockstyle  point = div [css [(blockstyle point)]][]

drawApple : Point -> Html msg
drawApple point = drawBlock apple point

drawSnakeBlock : Point -> Html msg
drawSnakeBlock point = drawBlock snakeBlock point

drawSnake : Snake -> List (Html msg)
drawSnake snake = List.map drawSnakeBlock snake.body


draw : Game.State -> Html msg
draw state =
  div
    [ css
      [ game 60 40 20 ]
    ]
    <| [drawApple state.apple] ++ drawSnake state.snake
