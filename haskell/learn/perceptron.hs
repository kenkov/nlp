import Control.Applicative
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Vector
import System.IO
import SVM

type Count = Int

findMaxR :: [Point] -> Double
findMaxR ps = maximum [l2Norm (point y) | y <- ps]

learn1 :: LearningRate -> Double -> Parameter -> Point -> Parameter
learn1 eta r2 (w, b) (Point x y) =
    if y * ((w |*| x) + b) <= 0
        then ([i + eta * j * y | (i, j) <- zip w x], b + eta * y * r2)
        else (w, b)

perceptron_ :: LearningRate -> Double -> [Point] -> Parameter -> Parameter
perceptron_ eta r2 ps (w, b) = foldl (learn1 eta r2) (w, b) ps

rep :: (a -> a) -> a -> [b] -> a
rep f x = foldl (\x y -> f x) x

perceptron :: Count -> LearningRate -> Dimension -> [Point] -> Parameter
perceptron count eta dim ps =
    let r2 = findMaxR ps in
        rep (perceptron_ eta r2 ps) (replicate dim 0, 0) [1..count]

batchPerceptron :: Count -> LearningRate -> Dimension -> [Point] -> Parameter
batchPerceptron = perceptron

onlinePerceptron :: LearningRate -> Dimension -> [Point] -> Parameter
onlinePerceptron = batchPerceptron 1


main :: IO ()
main = do
    -- let points = [Point [0, 0] (-1),
    --               Point [1, 0] (-1),
    --               Point [0, 1] 1,
    --               Point [1, 1] 1]
    points <- map (mkPoint . map read . words) . lines <$> getContents
    let eta = 0.1
    let dimension = 2
    let count = 100
    -- putStrLn $ showParameter $ onlinePerceptron eta dimension points
    putStrLn $ showParameter $ batchPerceptron count eta dimension points
