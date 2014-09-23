module SVM (
    Parameter,
    Point(..),
    LearningRate,
    mkPoint,
    showParameter,
) where

import Vector
import Data.List

type Parameter = (Vector, Double)
type LearningRate = Double

data Point = Point {
    point :: Vector,
    value :: Double
} deriving (Eq)

instance Show Point where
    show (Point xs y) = intercalate " " $ map show (xs ++ [y])


mkPoint :: [Double] -> Point
mkPoint xs = Point (init xs) (last xs)

showParameter :: Parameter -> String
showParameter (w, s) = intercalate " " $ map show (w ++ [s])
