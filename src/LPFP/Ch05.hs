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
cycle' :: [a] -> [a]
cycle' [] = error "Empty list cannot be cycled"
cycle' as = concat (repeat as)

runEx_5_9 :: IO ()
runEx_5_9 = do
    putStrLn "Exercise 5.9 Results:"
    putStrLn $
        let cycled' = cycle' [1,2,3]
        in "First 10 elements of \"cycle' [1,2,3]\":    " ++ show (take 10 cycled') ++ "\n"

--------------------
-- * Exercise 5.10 *
--------------------
-- Evaluate validity and types of various Haskell expressions
runEx_5_10 :: IO ()
runEx_5_10 = do
    putStrLn "Exercise 5.10 Results:"
    putStrLn "Which of the following are valid Haskell expressions?"
    putStrLn "a) [\"hello\", 42]"
    putStrLn "   INVALID: List elements must have the same type."
    putStrLn "   \"hello\" is String, 42 is Num a => a"
    putStrLn "b) ['h', \"ello\"]"
    putStrLn "   INVALID: List elements must have the same type."
    putStrLn "   'h' is Char, \"ello\" is String ([Char])"
    putStrLn "c) ['a', 'b', 'c']"
    putStrLn "   VALID: Type is [Char]"
    putStrLn $ "   Value: " ++ show ['a', 'b', 'c']
    putStrLn "d) length ['w', 'h', 'o']"
    putStrLn "   VALID: Type is Int"
    putStrLn $ "   Value: " ++ show (length ['w', 'h', 'o'])
    putStrLn "e) length \"hello\""
    putStrLn "   VALID: Type is Int"
    putStrLn $ "   Value: " ++ show (length "hello")
    putStrLn "f) reverse"
    putStrLn "   VALID: Type is [a] -> [a]"
    putStrLn "   This is a function that reverses any list\n"


--------------------
-- * Exercise 5.11 *
--------------------
runEx_5_11 :: IO ()
runEx_5_11 = do
    putStrLn "Exercise 5.11 Results:"
    putStrLn $ "[0,3..7.4] evaluates to: " ++ show ([0,3..7.4])
    putStrLn $ "[0,3..7.5] evaluates to: " ++ show ([0,3..7.5])
    putStrLn $ "[0,3..7.6] evaluates to: " ++ show ([0,3..7.6])
    putStrLn $ "[0,2..6.9] evaluates to: " ++ show ([0,2..6.9])
    putStrLn $ "[0,2..7.0] evaluates to: " ++ show ([0,2..7.0])
    putStrLn $ "[0,2..7.1] evaluates to: " ++ show ([0,2..7.1])
    putStrLn "In an arithmetic sequence, the sequence will terminate prior to the last element `n` if `n` is 1) not a whole number, and 2) the difference between `n` and the previous element is greater than half the sequence step size.\n"


--------------------
-- * Exercise 5.12 *
--------------------
eulerSum :: R
eulerSum = sum [1 / n**2 | n <- [1,2..100]]

runEx_5_12 :: IO ()
runEx_5_12 = do
    putStrLn "Exercise 5.12 Results:"
    let e = eulerSum
    putStrLn $ "Sum of (1 / n**2) from n=1 to 100:    " ++ show e ++ "\n"


--------------------
-- * Exercise 5.13 *
--------------------
fact :: Integer -> Integer
fact n = product [n,n-1..1]

runEx_5_13 :: IO ()
runEx_5_13 = do
    putStrLn "Exercise 5.13 Results:"
    putStrLn $
        let goodResult = 24
            ok = goodResult == fact 4
        in "`fact 4` == 4! == 24:   " ++ show ok ++ "\n"

--------------------
-- * Exercise 5.14 *
--------------------
expList :: R -> [R]
expList x = [(1 + x / fromIntegral n) ** fromIntegral n | n <- [1..]]

findNLimit :: R -> Int
findNLimit x = head [n | (n, approx) <- zip [1..] (expList x),
                         abs (approx - exp x) / exp x < 0.01]

runEx_5_14 :: IO ()
runEx_5_14 = do
    putStrLn "Exercise 5.14 Results:"
    putStrLn $
        let n = findNLimit 1
        in "Position of first element of `expList 1` within 1% of `exp 1`:    " ++ show n
    putStrLn $
        let n = findNLimit 10
        in "Position of first element of `expList 10` within 1% of `exp 10`:    " ++ show n ++ "\n"

--------------------
-- * Exercise 5.15 *
--------------------
expSeries :: R -> [R]
expSeries x = [x ** (fromIntegral m) / fromIntegral (fact m) | m <- [0..]]

expApprox :: R -> [R]
expApprox x = sums (expSeries x) 0
  where
    sums [] _ = []
    sums (y:ys) acc = let new = acc + y
                      in new:(sums ys new)

findNTaylor :: R -> Int
findNTaylor x = head [n | (n, approx) <- zip [1..] (expApprox x),
                          abs (approx - exp x) / exp x < 0.01]

runEx_5_15 :: IO ()
runEx_5_15 = do
    putStrLn "Exercise 5.15 Results:"
    putStrLn $
        let n = findNTaylor 1
        in "Position of first element of Taylor series for exp(1) within 1%:    " ++ show n
    putStrLn $
        let n = findNTaylor 10
        in "Position of first element of Taylor series for exp(10) within 1%:    " ++ show n
