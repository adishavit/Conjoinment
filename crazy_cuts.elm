import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random exposing (float, generate, initialSeed)

main : Element
main =
--   show <| flatten <| toListOfPairs <| appendFst <| crazyQuad  
--{-
  collage 400 400
    [ crazyPoly
      |> outlined defaultLine
        --|> filled blue
        --|> move (-10,0)
        --|> scale 20
    ]
--}
-- convert to HTML Polygon
crazyPoly = crazyQuad 
         |> flatten
         |> scalePts 100 
         |> polygon


crazyQuad = genCrazyQuad 42 |> toListOfPairs


genCrazyQuad seed = 
    let seed0 = initialSeed seed
        (x0,seed1) = generate (float -1 0) seed0
        (y0,seed2) = generate (float  0 1) seed1
        (x1,seed3) = generate (float  0 1) seed2
        (y1,seed4) = generate (float  0 1) seed3
        length = norm ((x1,x2) `minus` (x0,y0))
        (x2,seed5) = generate (float  0 1) seed4
        (y2,seed6) = generate (float -1 0) seed5
        (x3,y3)    = (x2 - length, y2)        
    in 
        --[ (-1,1), (1,1), (1,-1), (-1,-1) ]
        [(x0,y0), (x1,y1), (x2,y2), (x3,y3)]
        

-- transformation functions
-- type alias Point = (Float, Float)
scale s (x,y) = (s*x, s*y)
move (dx,dy) (x,y) = (x + dx, y + dy)
rotate rads (x,y) = (cos(rads)*x - sin(rads)*y, sin(rads)*x + cos(rads)*y)
plus (x0,y0) (x1,y1) = (x0+x1, y0+y1)
minus (x0,y0) (x1,y1) = (x0-x1, y0-y1)
norm (x,y) = sqrt (x*x+y*y)
normalize pt = scale (1.0 / norm pt) pt
normal (x,y) = normalize (-y, x)


{-
toCrazyQuad : List Point -> List List a 
toCrazyQuad ptListQuad = 
    let
        withFirstAsLast : List Point
        withFirstAsLast = ptListQuad ++ [head ptListQuad]  -- add head to back of list
    in
        case ptListQuad of
            [] -> []
            head::tail -> []
-}
 
appendFst lst = 
    case lst of 
        head::tail -> lst ++ [ head ]
        [] -> []


toListOfPairs lst = 
    case lst of
        [] -> []
        head::tail -> 
            case tail of 
                [] -> []
                nxt::rest -> [head, nxt] :: toListOfPairs tail


flatten lst =
    case lst of
        [] -> []
        head::tail -> head ++ flatten tail    
            
scalePts s pts = map (scale s) pts
