module LPFP.Ch05 where

import Data.Typeable
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

--------------------
-- * Exercise 5.3 *
--------------------
runEx_5_3 :: IO ()
runEx_5_3 = do
    let expression = "Hello, world!"
    putStrLn "Exercise 5.3 Results:"
    putStrLn $ "Expression: " ++ show expression
    putStrLn $ "Type of expression: " ++ show (typeOf expression)
    putStrLn $ "Value of expression: " ++ show (length expression)

--------------------
-- * Exercise 5.4 *
--------------------
myFunc :: Int -> [Int]
myFunc n = take n (repeat n)

runEx_5_4 :: IO ()
runEx_5_4 = do
    putStrLn "Exercise 5.4 Results:"
    putStrLn "`myFunc` takes an integer argument as input and returns a list consisting of the integer repeated a number of times equal to its value."
    putStrLn $
      let goodResult = [4, 4, 4, 4]
          ok = myFunc 4 == goodResult
      in "`myFunc 4` == [4, 4, 4, 4]:    " ++ show ok

--------------------
-- * Exercise 5.5 *
--------------------
null' :: [a] -> Bool
null' as = length as == 0

runEx_5_5 :: IO ()
runEx_5_5 = do
    putStrLn "Exercise 5.5 Results:"
    putStrLn $
        let ok = null' [] == True
        in "null' [] == True:    " ++ show ok
    putStrLn $
        let ok = null' [0] == False
        in "null' [0] == False:    " ++ show ok

