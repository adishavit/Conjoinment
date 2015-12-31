import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random exposing (float, generate, initialSeed)

main : Element
main =
  collage 300 300
    [ crazyPoly
      |> outlined defaultLine
        --|> filled blue
        --|> move (-10,0)
        --|> scale 20
    ]



{-
gen = Random.float 0 1
seed0 = Random.initialSeed 42
v = generate gen seed0
-}

-- convert to HTML Polygon
crazyPoly = polygon crazyQuad


crazyQuad = square |> scalePts 30


square = [ (-1,1), (1,1), (1,-1), (-1,-1) ]


-- transformation functions
--type alias Point = (Float, Float)
scale s (x,y) = (s*x, s*y)
move (dx,dy) (x,y) = (x + dx, y + dy)
rotate rads (x,y) = (cos(rads)*x - sin(rads)*y, sin(rads)*x + cos(rads)*y)
plus (x0,y0) (x1,y1) = (x0+x1, y0+y1)
minus (x0,y0) (x1,y1) = (x0-x1, y0-y1)
norm (x,y) = sqrt (x*x+y*y)
normalize pt = scale (1.0 / norm pt) pt
normal (x,y) = normalize (-y, x)
--  let (x,y) = pt0 `minus` pt1 in



scalePts s pts = map (scale s) pts
