module LPFP.Ch02 where

earthG :: Double
earthG = 9.807

-------------------
-- * Exercise 2.1 *
-------------------
f' :: Double -> Double
f' = sqrt . (+ 1)

runEx_2_1 :: IO ()
runEx_2_1 = do
    putStrLn $
      let x = f' 0.0
          ok = x == 1.0
      in  "f'(0) = " ++ show x ++ "    " ++ show ok
    putStrLn $
      let x = f' 1.0
          ok = x > 1.414 && x < 1.415
      in  "f'(1) = " ++ show x ++ "    " ++ show ok
    putStrLn $
      let x = f' 3.0
          ok = x == 2.0
      in  "f'(3) = " ++ show x ++ "    " ++ show ok


-------------------
-- * Exercise 2.2 *
-------------------
yRock30 :: Double -> Double
yRock30 t = 0.5 * (-earthG) * t ** 2 + 30 * t

runEx_2_2 :: IO()
runEx_2_2 = do
    putStrLn $
      let y = yRock30 1
      in "yRock30 1 = " ++ show y


-------------------
-- * Exercise 2.3 *
-------------------
vRock30 :: Double -> Double
vRock30 t = 30 + (-earthG) * t

runEx_2_3 :: IO()
runEx_2_3 = do
    putStrLn $
      let v = vRock30 1
      in "vRock30 1 = " ++ show v


-------------------
-- * Exercise 2.4 *
-------------------
sinDeg :: Double -> Double
sinDeg theta = sin $ theta * pi / 180

runEx_2_4 :: IO()
runEx_2_4 = do
    putStrLn $
      let s = sinDeg 30
          ok = 0.49999 < s && s < 0.5
      in "sinDeg 30 = " ++ show s ++ "    " ++ show ok


-------------------
-- * Exercise 2.5 *
-------------------
f :: Double -> Double
f x = x ** (1/3)

g :: Double -> Double
g y = exp y + 8 ** y

h :: Double -> Double
h x = 1 / (sqrt ((x - 5)**2 + 16))

gamma :: Double -> Double
gamma b = 1 / (sqrt (1 - b**2))

u :: Double -> Double
u x = 1 / (10 + x) + 1 / (10 - x)

l :: Double -> Double
l x = sqrt (x * (x + 1))

e :: Double -> Double
e x = 1 / (abs x)**3

e' :: Double -> Double
e' z = 1 / (z**2 + 4)**(3/2)


-------------------
-- * Exercise 2.5 *
-------------------
-- lambda for gamma function
gammaAnon = \b -> 1 / (sqrt (1 - b**2))
gammaAnon :: Double -> Double

runEx_2_6 :: IO()
runEx_2_6 = do
    putStrLn $
      let ga = gammaAnon 0.8
          ok = 1.666 < ga && ga < 1.667
      in "gammaAnon 0.8 = " ++ show ga ++ "    " ++ show ok
