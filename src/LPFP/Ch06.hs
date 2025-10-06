module LPFP.Ch06 where

import Data.Typeable
type R = Double

earthG :: R
earthG = 9.807

--------------------
-- * Exercise 6.1 *
--------------------
yRock :: R -> R -> R
yRock v0 t = 0.5 * (-earthG) * t ** 2 + v0 * t

vRock :: R -> R -> R
vRock v0 t = v0 - earthG * t

runEx_6_1 :: IO ()
runEx_6_1 = do
    putStrLn "Exercise 6.1 Results:"
    putStrLn $ "Type of yRock:    " ++ show (typeOf yRock)
    putStrLn $ "Type of vRock:    " ++ show (typeOf vRock) ++ "\n"

--------------------
-- * Exercise 6.2 *
--------------------
runEx_6_2 :: IO ()
runEx_6_2 = do
    putStrLn "Exercise 6.2 Results:"
    putStrLn "Type signature of patially applied function `take 4`:"
    putStrLn "take 4 :: [a] -> [a]\n"

--------------------
-- * Exercise 6.3 *
--------------------
runEx_6_3 :: IO ()
runEx_6_3 = do
    putStrLn "Exercise 6.3 Results:"
    -- can `not` be first arg to `map`? if so, what is type of `map not`?
    putStrLn "not :: Bool -> Bool"
    putStrLn "map :: (a -> b) -> [a] -> [b]"
    putStrLn "`not` can be 1st arg to `map`, restulting in type of `map not`:"
    putStrLn "(map not) :: [Bool] -> [Bool]"
