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

--------------------
-- * Exercise 4.2 *
--------------------
-- Function f(x) = x^3
f :: R -> R
f x = x ** 3

-- Exact derivative of f(x)
df :: R -> R
df x = 3 * x ** 2

-- Calculate error for a given x and step size a
derivativeError :: R -> R -> R
derivativeError x a = abs ((derivative a f x) - (df x))

findAFor1PercentError :: R -> R
findAFor1PercentError x =
    head [a | a <- [0.0001, 0.0002..1], derivativeError x a <= 0.01 * abs (df x)]

runEx_4_2 :: IO ()
runEx_4_2 = do
    putStrLn "Exercise 4.2 Results:"

    putStrLn "Errors for different x values with a = 1:"
    print $ derivativeError 1 1
    print $ derivativeError 2 1
    print $ derivativeError 3 1
    print $ derivativeError 4 1

    putStrLn "Errors for x = 4 with different a values:"
    print $ derivativeError 4 1
    print $ derivativeError 4 0.1
    print $ derivativeError 4 0.01
    print $ derivativeError 4 0.001

    putStrLn "Value of a for 1% error at x = 4:"
    print $ findAFor1PercentError 4

    putStrLn "Value of a for 1% error at x = 0.1:"
    print $ findAFor1PercentError 0.1

--------------------
-- * Exercise 4.3 *
--------------------
-- f(x) = x^4
f_4_3 :: R -> R
f_4_3 x = x ** 4

-- Exact derivative of f(x) = x^4
df_4_3 :: R -> R
df_4_3 x = 4 * x ** 3

-- Calculate relative error for a given x and step size a
relativeError :: R -> R -> R
relativeError x a =
    abs ((derivative a f_4_3 x - df_4_3 x) / df_4_3 x)

runEx_4_3 :: IO ()
runEx_4_3 = do
    putStrLn "Exercise 4.3 Results:"
    let x = 0.01  -- Independent variable x value
    let a = 0.01 -- Step size
    let error = relativeError x a
    putStrLn $ "Function: f(x) = x^4"
    putStrLn $ "x = " ++ show x
    putStrLn $ "Step size (a) = " ++ show a
    putStrLn $ "Relative error: " ++ show error
    putStrLn $ "Error percentage: " ++ show (error * 100) ++ "%"
    if error > 0.1
        then putStrLn "The error is more than 10%"
        else putStrLn "The error is NOT more than 10%"
