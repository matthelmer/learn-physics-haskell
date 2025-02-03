module LPFP.Ch03 where


-------------------
-- * Exercise 3.1 *
-------------------

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

-------------------
-- * Exercise 3.2 *
-------------------
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
