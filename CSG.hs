module CSG (
    Vector(..),
    Vertex(..)
) where


data Vector = Vector Float Float Float deriving (Eq, Show, Read)

origin :: Vector
origin = Vector 0 0 0

xaxis :: Vector
xaxis = Vector 1 0 0

yaxis :: Vector
yaxis = Vector 0 1 0

zaxis :: Vector
zaxis = Vector 0 0 1

dot :: Vector -> Vector -> Float
(Vector x1 y1 z1) `dot` (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

cross :: Vector -> Vector -> Vector
(Vector x1 y1 z1) `cross` (Vector x2 y2 z2) = Vector x y z
    where 
        x = y1 * z2 - z1 * y2
        y = z1 * x2 - x1 * z2
        z = x1 * y2 - y1 * x2

len :: Vector -> Float
len (Vector x y z) = sqrt (x*x + y*y + z*z)

times :: Vector -> Float -> Vector
(Vector x y z) `times` v = Vector (x*v) (y*v) (z*v)

minus :: Vector -> Vector -> Vector
(Vector x1 y1 z1) `minus` (Vector x2 y2 z2) = Vector (x1-x2) (y1-y2) (z1-z2)

plus :: Vector -> Vector -> Vector
(Vector x1 y1 z1) `plus` (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

lerp :: Vector -> Vector -> Float -> Vector
lerp a b v = a `plus` ((b `minus` a) `times` v)

dividedBy :: Vector -> Float -> Vector
(Vector x y z) `dividedBy` v = Vector (x/v) (y/v) (z/v)

rev :: Vector -> Vector
rev (Vector x y z) = (Vector (negate x) (negate y) (negate z))



data Vertex = Vertex { pos :: Vector, norm :: Vector } deriving (Eq, Show, Read)

vertexFlip :: Vertex -> Vertex
vertexFlip (Vertex pos norm) = Vertex pos (rev norm)

vertexLerp :: Vertex -> Vertex -> Float -> Vertex
vertexLerp (Vertex p1 n1) (Vertex p2 n2) t = Vertex (lerp p1 p2 t) (lerp n1 n2 t)



data Plane = Plane Vector Float deriving (Eq, Show, Read)

planeFlip :: Plane -> Plane 
planeFlip (Plane n w) = Plane (rev n) (negate w)



splitPolygon :: Polygon -> Plane -> ([Polygon], [Polygon], [Polygon], [Polygon])
splitPolygon pg pl = ([],[],[],[])
-- TODO


data Polygon = Polygon [Vertex] Plane deriving (Eq, Show, Read)






data BSPNode = EmptyNode | BSPNode 







