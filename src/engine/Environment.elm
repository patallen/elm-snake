
module Engine.Environment exposing
  ( Environment
  , environ
  , wrapXY
  )


import Engine.Vector exposing (Vec2i, vec2, mod2)
import Engine.Types exposing (Bounds2, BlockSize)

type alias Bounds2i = Bounds2 Int

type alias Environment =
  { bounds: Bounds2i
  }


environ : Int -> Int -> Int -> Environment
environ wdth hght blksize =
  { bounds = vec2 (vec2 0 0) (vec2 wdth hght)
  }

wrapXY : Environment -> Vec2i -> Vec2i
wrapXY {bounds} blk = let (l, h) = bounds in mod2 h blk
