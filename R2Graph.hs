---------------------------------
-- Geometry of R2 Plane
---------------------------------
module R2Graph (
    R2Vector(..),
    R2Point(..),
    Triangle(..),
    normal,
    normalize,
    addVectors,
    subtractVectors,
    multiplyVector,
    vectorLength,
    scalarProduct,
    parallelogramSignedArea,
    parallelogramArea,
    addPointAndVector,
    subtractPoints,
    distance,
    triangleSignedArea,
    triangleArea,
    intersectLines
) where

data R2Vector = R2Vector {
    cx :: Double,
    cy :: Double
} deriving (Show, Eq, Ord)

data R2Point = R2Point {
    x :: Double,
    y :: Double
} deriving (Show, Eq, Ord)

data Triangle = Triangle {
    vertex0 :: R2Point,
    vertex1 :: R2Point,
    vertex2 :: R2Point
} deriving (Show, Eq, Ord)

vectorLength :: R2Vector->Double
vectorLength v = sqrt ((cx v)^2 + (cy v)^2)

normal :: R2Vector->R2Vector
normal v = R2Vector (-cy v) (cx v)

normalize :: R2Vector->R2Vector
normalize v =
    let l = vectorLength v in
    if l == 0
    then v
    else R2Vector ((cx v)/l) ((cy v)/l)

addVectors :: R2Vector->R2Vector->R2Vector
addVectors u v = R2Vector (cx u + cx v) (cy u + cy v)

subtractVectors :: R2Vector->R2Vector->R2Vector
subtractVectors u v = R2Vector (cx u - cx v) (cy u - cy v)

multiplyVector :: R2Vector->Double->R2Vector
multiplyVector v a = R2Vector (cx v * a) (cy v * a)

scalarProduct :: R2Vector->R2Vector->Double
scalarProduct u v = cx u * cx v + cy u * cy v

parallelogramSignedArea :: R2Vector->R2Vector->Double
parallelogramSignedArea u v = (cx u * cy v) - (cy u * cx v)

parallelogramArea :: R2Vector->R2Vector->Double
parallelogramArea u v = abs $ parallelogramSignedArea u v

addPointAndVector :: R2Point->R2Vector->R2Point
addPointAndVector p v = R2Point (x p + cx v) (y p + cy v)

subtractPoints :: R2Point->R2Point->R2Vector
subtractPoints p q = R2Vector (x p - x q) (y p - y q)

distance :: R2Point->R2Point->Double
distance p q = vectorLength $ subtractPoints q p

triangleSignedArea :: Triangle->Double
triangleSignedArea t = 
    0.5 * parallelogramSignedArea 
    (subtractPoints (vertex1 t) (vertex0 t))
    (subtractPoints (vertex2 t) (vertex0 t))

triangleArea :: Triangle->Double
triangleArea t = abs $ triangleSignedArea t 

intersectLines :: R2Point->R2Vector->R2Point->R2Vector->Maybe R2Point
intersectLines p1 v1 p2 v2 =
    let
    n2 = normal v2
    denominator = scalarProduct n2 v1
    in
    if denominator /= 0
    then 
        let 
        t = (scalarProduct n2 (subtractPoints p2 p1)) / denominator
        in 
        Just (addPointAndVector p1 (multiplyVector v1 t))
    else 
        Nothing

