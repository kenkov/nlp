import Control.Applicative
import Data.STRef
import Control.Monad
import Control.Monad.ST


type Vector = [Double]

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

perceptron_ :: Double -> [Point] -> (Vector, Double) -> (Vector, Double)
perceptron_ r2 ps (w, b) = foldl f (w, b) ps
    where
        f (w', b') pt = if value pt * (innerProduct w' (point pt)) <= 0
            then ([i + j * (value pt) | (i, j) <- zip w' (point pt)], b + (value pt) * r2)
            else (w', b')

rep :: (a -> a) -> a -> [b] -> a
rep f x ys = foldl (\x -> \y -> f x) x ys

perceptron :: Int -> [Point] -> (Vector, Double)
perceptron dim ps = 
    let r2 = findMaxR ps in
        rep (perceptron_ r2 ps) (replicate dim 0, 0) [1..10000]


main :: IO ()
main = do
    let points = [Point [0, 0] (-1),
                  Point [1, 0] (-1),
                  Point [0, 1] 1,
                  Point [1, 1] 1]
    print $ perceptron 2 points
