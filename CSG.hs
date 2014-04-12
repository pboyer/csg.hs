module CSG (
    Vector(..),
    Vertex(..)
) where

import Data.Bits

class Invert a where
    invert :: a -> a

data Vector = Vector Float Float Float deriving (Eq, Show, Read)

instance Invert Vector where
    invert (Vector x y z) = (Vector (negate x) (negate y) (negate z))

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



data Vertex = Vertex { pos :: Vector, norm :: Vector } deriving (Eq, Show, Read)

instance Invert Vertex where
    invert (Vertex pos norm) = Vertex pos (invert norm)

vertexLerp :: Vertex -> Vertex -> Float -> Vertex
vertexLerp (Vertex p1 n1) (Vertex p2 n2) t = Vertex (lerp p1 p2 t) (lerp n1 n2 t)



data Plane = Plane Vector Float deriving (Eq, Show, Read)

instance Invert Plane where
    invert (Plane n w) = Plane (invert n) (negate w)

data Loc = Coplanar | Front | Back | Spanning deriving (Eq, Show, Read)

planeEpsilon = 0.00001

vertexLoc :: Plane -> Vertex -> Loc
vertexLoc (Plane n w) (Vertex vp vn)
    | t < (negate planeEpsilon) = Back
    | t > planeEpsilon = Front
    | otherwise = Coplanar
    where t = (n `dot` vp) - w

locMap :: (Bits a, Num a) => Loc -> a
locMap m
    | m == Coplanar = 0
    | m == Front = 1
    | m == Back = 2
    | m == Spanning = 3

locInv :: (Bits a, Num a) => a -> Loc
locInv l
    | l == 0 = Coplanar
    | l == 1 = Front
    | l == 2 = Back
    | l == 3 = Spanning

polygonLoc :: Plane -> Polygon -> Loc
polygonLoc (Plane n w) (Polygon vs pl) = locInv $ foldl (\a v -> a .|. (locMap $ vertexLoc pl v)) (0 :: Int) vs

data Edge = Edge Vertex Vertex 

enumEdges :: [Vertex] -> [Edge]
enumEdges vs = enumEdgesInternal vs vs

enumEdgesInternal :: [Vertex] -> [Vertex] -> [Edge]
enumEdgesInternal [] _ = []
enumEdgesInternal (v : []) va = [(Edge v (head va))] -- close the loop
enumEdgesInternal (v1 : v2 : vs) va = (Edge v1 v2) : (enumEdgesInternal (v2 : vs) va)

sameSide :: Plane -> Vertex -> Vertex -> Bool
sameSide pl v1 v2 = ((v1 >| pl) && (v2 >| pl)) || ((v1 <| pl) && (v2 <| pl))

(>|) :: Vertex -> Plane -> Bool
v >| p = (loc == Front) || (loc == Coplanar)
    where loc = vertexLoc p v

(<|) :: Vertex -> Plane -> Bool
v <| p = (loc == Back) || (loc == Coplanar)
    where loc = vertexLoc p v

planeInter :: Plane -> Vertex -> Vertex -> Vertex
planeInter (Plane n w) v1@(Vertex p1 n1) v2@(Vertex p2 n2) = vertexLerp v1 v2 t
    where t = (w - (n `dot` p1)) / (n `dot` (p1 `minus` p2))

splitVerts :: Plane -> [Edge] -> [Vertex]
splitVerts _ [] = []
splitVerts pl ((Edge v1 v2) : es)
    | sameSide pl v1 v2 = v1 : (splitVerts pl es) 
    | otherwise = v1 : (planeInter pl v1 v2) : (splitVerts pl es)

triFan :: [Vertex] -> Plane -> [Polygon]
triFan (v : vs) pl = triFanInternal v vs pl

triFanInternal :: Vertex ->[Vertex] -> Plane -> [Polygon]
triFanInternal _ [] _ = []
triFanInternal _ (_ : []) _ = []
triFanInternal _ (_ : _ : []) _ = []
triFanInternal v (v1 : v2 : v3 : vs) pl = (Polygon [v1,v2,v3] pl) : (triFanInternal v (v2 : v3 : vs) pl)

allWithLoc :: Loc -> Plane -> [Vertex] -> [Vertex]
allWithLoc l pl vs = filter (coplanarOrSide l pl) vs

coplanarOrSide :: Loc -> Plane -> Vertex -> Bool
coplanarOrSide l pl v = loc == Coplanar || loc == l
    where loc = (vertexLoc pl v)

splitSpanningPolygon :: Plane -> Polygon -> ([Polygon], [Polygon])
splitSpanningPolygon pl (Polygon vs ppl) = ((triFan frontvs ppl), (triFan backvs ppl))
    where 
        edges = enumEdges vs
        splitvs = (splitVerts pl edges) 
        frontvs = (allWithLoc Front pl splitvs)
        backvs = (allWithLoc Back pl splitvs)

splitPolygon :: Plane -> Polygon -> SplitResult
splitPolygon pl@(Plane n w) po@(Polygon vs (Plane pon _))
    | loc == Coplanar = if (pon `dot` n) > 0 then (SplitResult [po] [] [] []) else (SplitResult [] [po] [] [])
    | loc == Front = SplitResult [] [] [po] []
    | loc == Back = SplitResult [] [] [] [po]
    | loc == Spanning = SplitResult [] [] sf sb
    where 
        loc = polygonLoc pl po
        (sf, sb) = splitSpanningPolygon pl po

data Polygon = Polygon [Vertex] Plane deriving (Eq, Show, Read)

instance Invert Polygon where
    invert (Polygon vs pl) = Polygon vs (invert pl)


data Node = EmptyNode | Node Plane Node Node [Polygon] deriving (Eq, Show)

instance Invert Node where
    invert EmptyNode = EmptyNode
    invert (Node p f b ps) = Node (invert p) (invert b) (invert f) (map invert ps)

data SplitResult = EmptySplitResult | SplitResult {coplanarFront :: [Polygon], coplanarBack :: [Polygon], front :: [Polygon], back :: [Polygon] }

mergeSplit :: SplitResult -> SplitResult -> SplitResult
mergeSplit EmptySplitResult sr = sr
mergeSplit sr EmptySplitResult = sr
mergeSplit (SplitResult w x y z) (SplitResult w1 x1 y1 z1) = SplitResult (w ++ w1) (x ++ x1) (y ++ y1) (z ++ z1)

foldSplit :: Plane -> SplitResult -> Polygon -> SplitResult
foldSplit pl sr p = mergeSplit sr $ splitPolygon pl p

clipPolygons :: Node -> [Polygon] -> [Polygon]
clipPolygons _ [] = []
clipPolygons EmptyNode ps = ps
clipPolygons (Node p f b nps) ps = fa ++ ba
    where
        (SplitResult coF coB f1 b1) = foldl (foldSplit p) EmptySplitResult ps
        fa = coF ++ f1
        ba = if b == EmptyNode then [] else coB ++ b1

clipTo :: Node -> Node -> Node
clipTo EmptyNode (Node p f b ps) = EmptyNode
clipTo (Node p f b ps) EmptyNode = EmptyNode 
clipTo n@(Node p f b ps) n1@(Node p1 f1 b1 ps1) = (Node p fn bn psn)
    where 
        psn = clipPolygons n1 ps
        fn = clipTo f n1
        bn = clipTo b n1

allPolygons :: Node -> [Polygon]
allPolygons EmptyNode = []
allPolygons (Node _ f b ps) = ps ++ (allPolygons f) ++ (allPolygons b)

buildNode :: Node -> [Polygon] -> Node
buildNode n [] = n 
buildNode EmptyNode ((Polygon vs pl):ps) = (Node pl f b ps) 
    where
        (SplitResult coF coB f1 b1) = foldl (foldSplit pl) EmptySplitResult ps
        ps = coF ++ coB
        f = buildNode EmptyNode f1
        b = buildNode EmptyNode b1
buildNode (Node pl f b ps) pps = (Node pl fn bn psn)
    where
        (SplitResult coF coB f1 b1) = foldl (foldSplit pl) EmptySplitResult pps
        fn = buildNode f f1
        bn = buildNode b b1
        psn = ps ++ coF ++ coB
