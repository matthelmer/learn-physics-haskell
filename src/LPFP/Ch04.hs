module LPFP.Ch04 where

type R = Double
type Position = R
type Time = R
type Velocity = R
type Acceleration = R

--------------------
-- * Exercise 4.1 *
--------------------
type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt x t = (x (t + dt / 2) - x (t - dt / 2)) / dt

runEx_4_1 :: IO ()
runEx_4_1 = do
    let f x = 1 / 2 * x ** 2
    putStrLn "Exercise 4.1 Results:"
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

--------------------
-- * Exercise 4.4 *
--------------------
-- exact derivative of cos(t) is -sin(t)
df_4_4 :: R -> R
df_4_4 t = -sin t

-- error between numerical and exact derivative
cosDerivativeError :: R -> R -> R
cosDerivativeError t a = abs ((derivative a cos t) - (df_4_4 t))

-- relative error
cosRelativeError :: R -> R -> R
cosRelativeError t a =
    let exactDeriv = df_4_4 t
    in if exactDeriv == 0
       then abs (derivative a cos t)  -- Handle division by zero
       else abs ((derivative a cos t - exactDeriv) / exactDeriv)

runEx_4_4 :: IO ()
runEx_4_4 = do
    putStrLn "Exercise 4.4 Results:"

    -- check sensitivity at different t values with fixed a
    putStrLn "\nError at different t values with fixed a = 0.1:"
    let a = 0.1
    let tValues = [0, pi/6, pi/4, pi/3, pi/2, pi, 3*pi/2, 2*pi]
    mapM_ (\t -> putStrLn $ "t = " ++ show t ++ ", error = " ++
                           show (cosDerivativeError t a)) tValues

    -- how large a can be at certain t values while maintaining accuracy
    putStrLn "\nLarge step sizes that still give good approximations:"
    let checkLargeA t =
            let goodAValues = filter (\a -> cosRelativeError t a < 0.01) [0.1, 0.5, 1.0, 2.0, 5.0, 10.0]
            in "t = " ++ show t ++ ", largest good a = " ++
               if null goodAValues then "none found" else show (maximum goodAValues)

    mapM_ (putStrLn . checkLargeA) [0, pi/4, pi/2, pi, 3*pi/2, 2*pi]

    -- Explore where the derivative is most/least sensitive
    putStrLn "\nSensitivity analysis across t values:"
    let sensitivityTest t =
            let smallChange = cosDerivativeError t 0.1 - cosDerivativeError t 0.01
            in "t = " ++ show t ++ ", sensitivity = " ++ show smallChange

    mapM_ (putStrLn . sensitivityTest) [0, pi/6, pi/4, pi/3, pi/2, pi, 3*pi/2, 2*pi]


--------------------
-- * Exercise 4.5 *
--------------------
pos1 :: Time -> Position
pos1 t = if t < 0
         then 0
         else 5 * t**2

-- define functions for vel and acc functions by taking an analytic derivative of position function
vel1Analytic :: Time -> Velocity
vel1Analytic t = if t < 0
                 then 0
                 else 10 * t

acc1Analytic :: Time -> Acceleration
acc1Analytic t = if t < 0
                 then 0
                 else 10

-- define functions for corresponding vel and acc functions by taking a numerical derivative of position function using derivative 0.01.
dt_4_5 :: R
dt_4_5 = 0.01

vel1Numerical :: Time -> Velocity
vel1Numerical t = derivative dt_4_5 pos1 t

acc1Numerical :: Time -> Acceleration
acc1Numerical t = derivative dt_4_5 vel1Numerical t

-- can you find any values of t where `vel1Analytic t` and `vel1Numerical t` differ substantially?
-- can you find any values of t where `acc1Analytic t` and `acc1Numerical t` differ substantially?

runEx_4_5 :: IO ()
runEx_4_5 = do
    putStrLn "Exercise 4.5 Results:"

    let testTimes = [-0.1, 0.0, 0.001, 0.005, 0.01, 0.1, 1.0]

    putStrLn "\nComparing Analytic vs. Numerical Velocity (dt = 0.01):"
    mapM_ (\t -> do
        let vAnalytic = vel1Analytic t
        let vNumerical = vel1Numerical t
        putStrLn $ "t = " ++ show t ++
                   ", V_analytic = " ++ show vAnalytic ++
                   ", V_numerical = " ++ show vNumerical ++
                   ", Diff = " ++ show (abs (vAnalytic - vNumerical))
        ) testTimes

    putStrLn "\nComparing Analytic vs. Numerical Acceleration (dt = 0.01):"
    -- Note: Analytic acceleration is undefined at t=0. The numerical value at t=0
    -- will be an approximation based on the velocity points around 0.
    mapM_ (\t -> do
        let aAnalytic = if t == 0 then nan else acc1Analytic t
        let aNumerical = acc1Numerical t
        putStrLn $ "t = " ++ show t ++
                   ", A_analytic = " ++ (if isNaN aAnalytic then "Undefined" else show aAnalytic) ++
                   ", A_numerical = " ++ show aNumerical ++
                   (if isNaN aAnalytic then "" else ", Diff = " ++ show (abs (aAnalytic - aNumerical)))
        ) testTimes


-- helper to produce NaN (Not a Number)
nan :: Double
nan = 0/0
