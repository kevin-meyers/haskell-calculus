module Main where

import Lib

main :: IO ()
main = someFunc

limit :: Double -> [Double] -> Double
limit eps (x:y:xs)
  | eps >= abs (y - x) = 0.0
  | otherwise = 1 + limit eps (y:xs)

nrSqrt :: Double -> Double
nrSqrt n = limit 0.01 (iterate next 1.0)
    where
        next x = (x + n/x) / 2

deriv :: (Double -> Double) -> Double -> Double
deriv f x = limit 0.0001 $ map slope $ iterate (/2) 1.0
    where
        slope h = (f (x+h) - f x) / h

improve :: Integer -> [Double] -> [Double]
improve n xs = refine xs $ map (^n) $ iterate (/2) 1.0

refine :: [Double] -> [Double] -> [Double]
refine (x1:x2:xs) (y1:y2:ys) = (x1 - b * y1) : refine (x2:xs) (y2:ys)
    where
        b = (x1 - x2) / (y1 - y2)

