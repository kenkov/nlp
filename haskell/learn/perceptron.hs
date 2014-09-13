import Control.Applicative
import Data.STRef
import Control.Monad
import Control.Monad.ST


type Vector = [Double]
type Parameter = (Vector, Double)
type Dimension = Int
type Count = Int

data Point = Point {
    point :: Vector,
    value :: Double
} deriving (Show, Eq)

innerProduct :: Vector -> Vector -> Double
innerProduct x y = sum $ zipWith (*) x y

l2Norm :: Vector -> Double
l2Norm x = sum $ map (^2) x

findMaxR :: [Point] -> Double
findMaxR ps = maximum [l2Norm (point y) | y <- ps]

learn :: Double -> Parameter -> Point -> Parameter
learn r2 (w, b) pt =
    if value pt * innerProduct w (point pt) <= 0
        then ([i + j * value pt | (i, j) <- zip w (point pt)], b + value pt * r2)
        else (w, b)

perceptron_ :: Double -> [Point] -> Parameter -> Parameter
perceptron_ r2 ps (w, b) = foldl (learn r2) (w, b) ps

rep :: (a -> a) -> a -> [b] -> a
rep f x = foldl (\x y -> f x) x

perceptron :: Count -> Dimension -> [Point] -> Parameter
perceptron count dim ps = 
    let r2 = findMaxR ps in
        rep (perceptron_ r2 ps) (replicate dim 0, 0) [1..count]

batchPerceptron :: Count -> Dimension -> [Point] -> Parameter
batchPerceptron = perceptron

onlinePerceptron :: Dimension -> [Point] -> Parameter
onlinePerceptron = batchPerceptron 1


main :: IO ()
main = do
    let points = [Point [0, 0] (-1),
                  Point [1, 0] (-1),
                  Point [0, 1] 1,
                  Point [1, 1] 1]
    print $ onlinePerceptron 2 points
    print $ batchPerceptron 10000 2 points
