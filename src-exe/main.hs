module Main where

import  RayT
import  RayT.Primitives
import  RayT.Image (traceToPng)

main :: IO()
main = do
	putStrLn "tracing image..."
	traceToPng sz cam testScene "test.png"
	putStrLn "...done!"
	where sz  = (600, 600)
	      cam = defaultCamera (-10) (2, 2)

testScene :: Scene
testScene = [ sphere redM c r ]
	where c = Vec3 (0,0,10)
	      r = 1
	      redM = Mat (rgb 1 0 0)