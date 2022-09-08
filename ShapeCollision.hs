{-
    An implementation of the SAT (Separating Axis Theorem) algorithm 
    which detects collisions between convex shapes.
-}

module ShapeCollision where

type Vertex = (Double, Double)

type Projection = Vertex

type Shape = [Vertex]

type Axes = Shape

-- Take a vector and return one in the same direction but with length 1
normalize :: Vertex -> Vertex 
normalize (v1,v2) = 
  let norm = sqrt (v1 ** 2 + v2 ** 2)
    in (v1 / norm, v2 / norm)

-- Dot product
dot :: Vertex -> Vertex -> Double
dot v1 v2 = (fst v1 * fst v2) + (snd v1 * snd v2)

-- Take the difference of 2 vertices to get an edge
getEdge :: Vertex -> Vertex -> Vertex
getEdge v1 v2 = (fst v1 - fst v2, snd v1 - snd v2)

-- cons the last of the list to the list itself,
-- allowing us to get the edge by taking the
-- difference of the last and the head of the list,
-- then proceed as normal to get the rest of the edges
-- in shapeToEdges.
transformShape :: Shape -> Shape
transformShape ss = last ss : ss

-- Get edges between each successive pair of edges 
shapeToEdges :: Shape -> Shape
shapeToEdges shape =
  let shape' = transformShape shape
   in zipWith getEdge shape' (tail shape')

-- Obtain axes by getting normal vectors for our shape's vertices
getAxis :: Vertex -> Vertex
getAxis (x, y) = (-y, x)

-- Obtain projection of a shape onto an axis 
project :: Shape -> Axes -> Projection
project vs (a : as) =
  let dots = foldr (\v acc -> (dot v a) : acc) [] vs
   in let min' = foldr1 min dots
       in let max' = foldr1 max dots
           in (min', max')

-- Determine whether two given projections are overlapping
isOverlapping :: Projection -> Projection -> Bool
isOverlapping p1 p2 =
  let min1 = fst p1
   in let max2 = snd p2
       in let min2 = fst p2
           in let max1 = snd p1
               in if fst p1 <= snd p2 && fst p2 <= snd p1 then True else False

-- Determine whether two shapes are colliding 
separatingAxisTheorem :: Shape -> Shape -> Bool
separatingAxisTheorem s1 s2 =
  let edges = (shapeToEdges s1) ++ (shapeToEdges s2)
   in let normalizedAxes = foldr (\e acc -> (normalize (getAxis e)) : acc) [] edges
       in foldr (\a acc -> if isOverlapping (project s1 a) (project s2 a) 
           then True else acc) False [normalizedAxes]

main :: IO ()
main = do
  putStrLn "Enter vertices for shape1 in the form [(x0,y0),(x1,y1),...(xi,yi)]"
  shape1 <- getLine
  let shape1' = read shape1 :: Shape
  putStrLn "Enter vertices for shape2 please"
  shape2 <- getLine
  let shape2' = read shape2 :: Shape
  let areColliding = separatingAxisTheorem shape1' shape2'
  if areColliding then putStrLn "Yes, they're colliding" 
    else putStrLn "No, they're not colliding"
