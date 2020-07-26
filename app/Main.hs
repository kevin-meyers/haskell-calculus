module Main where

import Data.Fixed (mod')

import Lib

main :: IO ()
main = someFunc


limit :: Double -> [Double] -> Double
limit eps (x:y:xs)
  | eps >= abs (y - x) = y
  | otherwise = limit eps (y:xs)

limitCount :: Double -> [Double] -> Double
limitCount eps (x:y:xs)
  | eps >= abs (y - x) = 1.0
  | otherwise = 1 + limitCount eps (y:xs)

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

integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = limit 0.001 $ improve 2 $ improve 1 $ map (integral' f a b) $ iterate (/2) 1.0

integral' :: (Double -> Double) -> Double -> Double -> Double -> Double
integral' f a b h = (+remainder) .  sum $ map ((*h) . f) $ makeRange h a b
    where
        remainder = mod' b h * f b

makeRange :: Double -> Double -> Double -> [Double]
makeRange stepSize a b 
  | b - a < stepSize = []
  | otherwise = a : makeRange stepSize (a + stepSize) b
