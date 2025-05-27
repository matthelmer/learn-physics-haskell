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

--------------------
-- * Exercise 5.2 *
--------------------
sndItem0 :: [a] -> a
sndItem0 xs = if null xs
              then error "Empty list has no second element."
              else if length xs == 1
                   then error "1-item list has no 2nd item."
                   else xs !! 1

runEx_5_2 :: IO ()
runEx_5_2 = do
    putStrLn "Exercise 5.2 Results:"
    putStrLn "Testing sndItem0:"
    let list1 = [1, 2, 3, 4]
    putStrLn $ "sndItem0 " ++ show list1 ++ " = " ++ show (sndItem0 list1)
