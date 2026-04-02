module LPFP.Ch07 where

import LPFP.Ch02 (yRock30)
import LPFP.Ch06 (yRock)
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

type R = Double


--------------------
-- * Exercise 7.1 *
--------------------
plot1 :: IO ()
plot1 = plotFunc [terminal (PNG.cons "plots/ch07/plot1.png")] ([-10,-9.9..10] :: [R]) sin

runEx_7_1 :: IO ()
runEx_7_1 = do
    putStrLn "Exercise 7.1 Results:"
    plot1
    putStrLn "'plots/ch07/plot1.png' saved"

--------------------
-- * Exercise 7.2 *
--------------------
plot2 :: IO ()
plot2 = plotFunc [terminal (PNG.cons "plots/ch07/plot2.png")] [0,0.1..6] yRock30

runEx_7_2 :: IO ()
runEx_7_2 = do
    putStrLn "Exercise 7.2 Results:"
    plot2
    putStrLn "'plots/ch07/plot2.png' saved"

--------------------
-- * Exercise 7.3 *
--------------------
plot3 :: IO ()
plot3 = plotFunc [terminal (PNG.cons "plots/ch07/plot3.png")] [0,0.1..4] (yRock 20)

runEx_7_3 :: IO ()
runEx_7_3 = do
    putStrLn "Exercise 7.3 Results:"
    plot3
    putStrLn "'plots/ch07/plot3.png' saved"
