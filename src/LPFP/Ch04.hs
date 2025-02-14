module LPFP.Ch04 where

type R = Double

--------------------
-- * Exercise 4.1 *
--------------------
type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt x t = (x (t + dt / 2) - x (t - dt / 2)) / dt

runEx_4_1 :: IO ()
runEx_4_1 = do
    let f x = 1 / 2 * x ** 2
    putStrLn "Derivatives of f(x) = 1/2 * x^2 at x = 1:"
    putStrLn $ "dt = 10:   " ++ show (derivative 10 f 1)
    putStrLn $ "dt = 1:    " ++ show (derivative 1 f 1)
    putStrLn $ "dt = 0.1:  " ++ show (derivative 0.1 f 1)
    putStrLn "Expected derivative at x = 1: 1.0"
