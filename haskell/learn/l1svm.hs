import Control.Applicative
import Vector
import System.IO
import System.Environment
import Data.List
import SVM

type Count = Int

grad :: Dimension -> Double -> Parameter -> Point -> Parameter
grad dim c (w, b) (Point x y) =
    (g1 |+| g2, b' + b'')
    where
        disc = y * ((w |*| x) + b) < 1.0
        g1 = if disc
                then scalaProduct (- y) x
                else replicate dim 0
        g2 = [if abs wi > 1.0e-5 then c * signum wi else 0 | wi <- x ]
        b' = if disc then -y else 0
        b'' = if abs b > 1.0e-5 then c * signum b else 0

learn1 :: LearningRate -> Dimension -> Double -> Parameter -> Point -> Parameter
learn1 eta dim c (w, b) pt =
    let (gw, gb) = grad dim c (w, b)  pt in
        (zipWith (\ x y -> x - eta * y) w gw, b - eta * gb)

l1svm_ :: LearningRate -> Dimension -> Double -> [Point] -> Parameter -> Parameter
l1svm_ eta dim c ps param = foldl (learn1 eta dim c) param ps

rep :: (a -> a) -> a -> [b] -> a
rep f x = foldl (\x y -> f x) x

l1svm :: Count -> LearningRate -> Dimension -> Double -> [Point] -> Parameter
l1svm count eta dim c ps = rep (l1svm_ eta dim c ps) (replicate dim 0, 0) [1..count]

batchL1svm :: Count -> LearningRate -> Dimension -> Double -> [Point] -> Parameter
batchL1svm = l1svm

onlineL1svm :: LearningRate -> Dimension -> Double -> [Point] -> Parameter
onlineL1svm = l1svm 1


main :: IO ()
main = do
    points <- map (mkPoint . map read . words) . lines <$> getContents
    c <- head . map read <$> getArgs
    -- let c = 1.0e-5
    let count = 100
    let dim = 2
    let eta = 0.1
    putStrLn $ showParameter $ batchL1svm count eta dim c points
    -- putStrLn $ showParameter $ onlineL1svm eta dim c points
