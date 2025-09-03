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
    putStrLn $ "numbers = " ++ show numbers ++ "\n"

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
    putStrLn $ "sndItem0 " ++ show list1 ++ " = " ++ show (sndItem0 list1) ++ "\n"

--------------------
-- * Exercise 5.3 *
--------------------
runEx_5_3 :: IO ()
runEx_5_3 = do
    let expression = "Hello, world!"
    putStrLn "Exercise 5.3 Results:"
    putStrLn $ "Expression: " ++ show expression
    putStrLn $ "Type of expression: " ++ show (typeOf expression)
    putStrLn $ "Value of expression: " ++ show (length expression) ++ "\n"

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
      in "`myFunc 4` == [4, 4, 4, 4]:    " ++ show ok ++ "\n"

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
        in "null' [0] == False:    " ++ show ok ++ "\n"

--------------------
-- * Exercise 5.6 *
--------------------
last' :: [a] -> a
last' as = if null as
           then error "Empty list has no last element"
           else head (reverse as)

runEx_5_6 :: IO ()
runEx_5_6 = do
    putStrLn "Exercise 5.6 Results:"
    -- Note: last' [] would throw an error, so we don't test it here
    putStrLn "last' [] would produce: \"Empty list has no last element\" error"
    -- Non-empty lists
    putStrLn $ "last' [1, 2]:    " ++ show (last' [1, 2])
    putStrLn $ "last' \"hello\":  " ++ show (last' "hello") ++ "\n"

--------------------
-- * Exercise 5.7 *
--------------------
palindrome :: String -> Bool
palindrome word = reverse word == word

runEx_5_7 :: IO ()
runEx_5_7 = do
    putStrLn "Exercise 5.7 Results:"
    putStrLn $
        let ok = palindrome "hello" == False
        in "\"hello\" is NOT a palindrome:    " ++ show ok
    putStrLn $
        let ok = palindrome "AJA" == True
        in "\"AJA\" is a palindrome:    " ++ show ok ++ "\n"

--------------------
-- * Exercise 5.8 *
--------------------
runEx_5_8 :: IO ()
runEx_5_8 = do
    putStrLn "Exercise 5.8 Results:"
    putStrLn "What are the first five elements of infinite list [9,1..]?"
    putStrLn $ show (take 5 [9,1..]) ++ "\n"


--------------------
-- * Exercise 5.9 *
--------------------


--------------------
-- * Exercise 5.10 *
--------------------


--------------------
-- * Exercise 5.11 *
--------------------


--------------------
-- * Exercise 5.12 *
--------------------


--------------------
-- * Exercise 5.13 *
--------------------


--------------------
-- * Exercise 5.14 *
--------------------


--------------------
-- * Exercise 5.15 *
--------------------
