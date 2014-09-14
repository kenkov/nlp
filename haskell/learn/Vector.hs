module Vector (
    Vector,
    Dimension,
    innerProduct,
    l2Norm,
) where

type Vector = [Double]
type Dimension = Int

innerProduct :: Vector -> Vector -> Double
innerProduct x y = sum $ zipWith (*) x y

l2Norm :: Vector -> Double
l2Norm x = sum $ map (^2) x
