module Vector (
    Vector,
    Dimension,
    innerProduct,
    (|*|),
    l2Norm,
    vectorSum,
    (|+|),
    scalaProduct,
    sign,
) where

type Vector = [Double]
type Dimension = Int

innerProduct :: Vector -> Vector -> Double
innerProduct x y = sum $ zipWith (*) x y

(|*|) :: Vector -> Vector -> Double
x |*| y = x `innerProduct` y

vectorSum :: Vector -> Vector -> Vector
vectorSum = zipWith (+)

(|+|) :: Vector -> Vector -> Vector
x |+| y = x `vectorSum` y

scalaProduct :: Double -> Vector -> Vector
scalaProduct x = map (*x)

l2Norm :: Vector -> Double
l2Norm x = sum $ map (^2) x

sign :: Double -> Double
sign x = if x >= 0 then 1.0 else -1.0
