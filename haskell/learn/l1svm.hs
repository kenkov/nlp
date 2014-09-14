import Control.Applicative
import Vector


type Parameter = (Vector, Double)
type Count = Int
type LearningRate = Double

data Point = Point {
    point :: Vector,
    value :: Double
} deriving (Show, Eq)

grad :: Dimension -> Double -> Parameter -> Point -> Parameter
grad dim c (w, b) (Point x y) =
    (vectorSum g1 g2, 0)
    where
        g1 = if y * innerProduct w x < 1.0
                then scalaProduct (- y) x
                else replicate dim 0
        g2 = [if abs wi > 1.0e-15 then c * y else 0 | wi <- x]

learn1 :: LearningRate -> Dimension -> Double -> Parameter -> Point -> Parameter
learn1 eta dim c (w, b) pt =
    let (gw, gb) = grad dim c (w, b)  pt in
        (zipWith (\ x y -> x - eta * y) w gw, gb + 0)

learn :: LearningRate -> Dimension -> Double -> [Point] -> Parameter
learn eta dim c = foldl (learn1 eta dim c) ([0, 0], 0)


main :: IO ()
main = do
    let points = [Point [0, 0] (-1),
                  Point [1, 0] (-1),
                  Point [2, 0] (-1),
                  Point [3, 0] (-1),
                  Point [0, 1] 1,
                  Point [1, 1] 1,
                  Point [1, 2] 1,
                  Point [1, 3] 1
                  ]
    let eta = 0.1
    let dim = 2
    let c = 0.1
    print $ learn eta dim c points
