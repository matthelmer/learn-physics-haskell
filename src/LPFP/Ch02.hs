module LPFP.Ch02 where

-- Chapter Code
earthG :: Double
earthG = 9.807

-- * Exercise 2.1 *
-------------------
runEx_2_1 :: IO ()
runEx_2_1 = do
    putStrLn $
      let x = f 0.0
          ok = x == 1.0
      in  "f(0) = " ++ show x ++ "    " ++ show ok
    putStrLn $
      let x = f 1.0
          ok = x > 1.414 && x < 1.415
      in  "f(1) = " ++ show x ++ "    " ++ show ok
    putStrLn $
      let x = f 3.0
          ok = x == 2.0
      in  "f(3) = " ++ show x ++ "    " ++ show ok

f :: Double -> Double
f = sqrt . (+ 1)


-- * Exercise 2.2 *
-------------------
yRock30 :: Double -> Double
yRock30 t = 0.5 * (-earthG) * t ** 2 + 30 * t

runEx_2_2 :: IO()
runEx_2_2 = do
    putStrLn $
      let y = yRock30 1
      in "yRock30 1 = " ++ show y


-- * Exercise 2.3 *
-------------------
vRock30 :: Double -> Double
vRock30 t = 30 + (-earthG) * t

runEx_2_3 :: IO()
runEx_2_3 = do
    putStrLn $
      let v = vRock30 1
      in "vRock30 1 = " ++ show v
