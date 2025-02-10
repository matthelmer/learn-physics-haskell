module LPFP.Ch03 where

--------------------
-- * Exercise 3.1 *
--------------------
runEx_3_1 :: IO ()
runEx_3_1 = do
    putStrLn $
      let x = False || True && False || True
          y = False || (True && False) || True
          ok = x == y
      in "a) x == y    " ++ show ok
    putStrLn $
      let x = 2 / 3 / 4 == 4 / 3 / 2
          y = ((2 / 3) / 4) == ((4 / 3) / 2)
          ok = x == y
      in "b) x == y    " ++ show ok
    putStrLn $
      let x = 7 - 5 / 4 > 6 || 2 ^ 5 - 1 == 31
          y = ((7 - (5 / 4)) > 6) || (((2 ^ 5) - 1) == 31)
          ok = x == y
      in "c) x == y    " ++ show ok
    putStrLn $
      let x = "(2 < 3) && (3 < 4)"
      in "d) Not well-formed, should be: " ++ x
    putStrLn $
      let x = 2 < 3 && 3 < 4
          y = (2 < 3) && (3 < 4)
          ok = x == y
      in "e) x == y    " ++ show ok
    putStrLn $
      "f) Not well-formed, AND operator requires both inputs be booleans"

--------------------
-- * Exercise 3.2 *
--------------------
f :: Double -> Double
f x = if x <= 0
        then 0
        else x

e :: Double -> Double
e r = if r <= 1
        then r
        else 1 / r**2

runEx_3_2 :: IO ()
runEx_3_2 = do
    putStrLn $
      let ok = f (-1) == 0
      in show ok
    putStrLn $
      let ok = f 1 == 1
      in show ok

--------------------
-- * Exercise 3.3 *
--------------------
isXorY :: Char -> Bool
isXorY myChar = case myChar of
                    'Y' -> True
                    'X' -> True
                    _   -> False

runEx_3_3 :: IO ()
runEx_3_3 = do
    putStrLn $
      let ok = isXorY 'X' == True
      in show ok
    putStrLn $
      let ok = isXorY 'Z' == False
      in show ok

--------------------
-- * Exercise 3.4 *
--------------------
bagFee :: Bool -> Int
bagFee checkingBags = if checkingBags
                        then 100
                        else 0

bagFee2 :: Bool -> Int
bagFee2 True = 100
bagFee2 _ = 0

runEx_3_4 :: IO ()
runEx_3_4 = do
    putStrLn $
      let ok = bagFee True == 100
      in show ok
    putStrLn $
      let ok = bagFee False == 0
      in show ok
    putStrLn $
      let ok = bagFee2 False == 0
      in show ok

--------------------
-- * Exercise 3.5 *
--------------------
greaterThan50 :: Integer -> Bool
greaterThan50 n = n > 50

runEx_3_5 :: IO ()
runEx_3_5 = do
    putStrLn $
      let ok = greaterThan50 50 == False
      in "50 is not greaterThan50   " ++ show ok
    putStrLn $
      let ok = greaterThan50 51 == True
      in "50 is greaterThan50   " ++ show ok

--------------------
-- * Exercise 3.6 *
--------------------
amazingCurve :: Int -> Int
amazingCurve score = case score of
                        greaterThan50 -> 100
                        _             -> 2 * score

runEx_3_6 :: IO ()
runEx_3_6 = do
    putStrLn $
      let ok = amazingCurve 1 == 2
      in "amazingCurve 1 == 2    " ++ show ok
    putStrLn $
      let ok = amazingCurve 50 == 100
      in "amazingCurve 50 == 100    " ++ show ok
    putStrLn $
      let ok = amazingCurve 51 == 100
      in "amazingCurve 51 == 100    " ++ show ok

--------------------
-- * Exercise 3.7 *
--------------------
runEx_3_7 :: IO ()
runEx_3_7 = do
    putStrLn $ "Type of 'bagFee False': Int"
    putStrLn $ "Value of 'bagFee False': " ++ show (bagFee False)

--------------------
-- * Exercise 3.8 *
--------------------


--------------------
-- * Exercise 3.9 *
--------------------
--------------------
-- * Exercise 3.10 *
--------------------
