module LPFP.Ch05 where

type R = Double

--------------------
-- * Exercise 5.1 *
--------------------
numbers :: [R]
numbers = [-2.0, -1.2..2.0]

runEx_5_1 :: IO ()
runEx_5_1 = do
    putStrLn "Exercise 5.1 Results:"
    putStrLn $ "numbers = " ++ show numbers
