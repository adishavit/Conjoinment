import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Array
import Maybe exposing (withDefault)

main : Element
import Impure

main =
    let 
        poly = toPolygon crazyQuad
        twin = toPolygon <| genTwin crazyQuad
    in
   --show <| genCrazyQuad 42 
   --show <|  crazyQuad
   --show <| poly  
--{-
    collage 400 400
        [ poly |> filled red
        , poly |> outlined { defaultLine | color = red }
        , twin |> filled red
        , twin |> outlined { defaultLine | color = red }
        ]
--}
-- convert to HTML Polygon
toPolygon poly = poly  
                |> flatten
                |> scalePath 100 
                |> polygon


crazyQuad = toPoly <| genCrazyQuad

genCrazyQuad = 
    let x0 = negate (Impure.getRandom ())
        y0 = 0.5 * (Impure.getRandom ())
        x1 = (Impure.getRandom ())
        y1 = 0.5 * (Impure.getRandom ())
        length = norm ((x1,y1) `minus` (x0,y0))
        x2 = (Impure.getRandom ())
        y2 = -0.5 * (Impure.getRandom ())
        (x3,y3)    = (x2 - length, y2)        
    in 
        --[ (-1,1), (1,1), (1,-1), (-1,-1) ]
        [(x0,y0), (x1,y1), (x2,y2), (x3,y3)]  

genTwin poly =
    let 
        getFirstLegPoint index pp = 
            Array.fromList pp   |>
            Array.get index     |>
            withDefault []      |>
            head                |>
            withDefault (0,0)  
              
        getOffset1 pp = neg <| getFirstLegPoint 0 pp          
        getAngle pp = 
            let (x1,y1) = getFirstLegPoint 1 pp 
            in -(atan2 y1 x1)       

        getOffset2 pp = getFirstLegPoint 3 pp 
        
        movedPoly = poly |> movePoly (getOffset1 poly)
        rotPoly = movedPoly |> rotatePoly (getAngle movedPoly) 
        movedPoly2 = movePoly (getOffset2 poly) rotPoly
    in
        movedPoly2
    
    


-- transformation functions
type alias Point = (Float, Float)
neg (x,y) = (-x,-y)
scale s (x,y) = (s*x, s*y)
move (dx,dy) (x,y) = (x + dx, y + dy)
rotate rads (x,y) = (cos(rads)*x - sin(rads)*y, sin(rads)*x + cos(rads)*y)
plus (x0,y0) (x1,y1) = (x0+x1, y0+y1)
minus (x0,y0) (x1,y1) = (x0-x1, y0-y1)
norm (x,y) = sqrt(x*x+y*y)
normalize pt = scale (1.0 / norm pt) pt
normal (x,y) = normalize (-y, x)

type alias Path = List Point
scalePath s pts = map (scale s) pts
movePath offset pts = map (move offset) pts
rotatePath angle pts = map (rotate angle) pts

type alias Poly = List Path
movePoly offset poly = map (movePath offset) poly
rotatePoly angle poly = map (rotatePath angle) poly



-- utils
appendFst lst = 
    case lst of 
        head::tail -> lst ++ [ head ]
        [] -> []


toPoly_rec lst = 
    case lst of
        [] -> []
        head::tail -> 
            case tail of 
                [] -> []
                nxt::rest -> [head, nxt] :: toPoly_rec tail

toPoly lst = 
    let 
        first = withDefault (0,0) (head lst)
        last = withDefault (0,0) (head (reverse lst))
    in
        toPoly_rec lst ++ [[last, first]]


flatten lst =
    case lst of
        [] -> []
        head::tail -> head ++ flatten tail    
            
