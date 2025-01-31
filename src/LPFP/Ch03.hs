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
