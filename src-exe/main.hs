module Main where

import  RayT
import  RayT.Primitives
import  RayT.Image (traceToPng)

main :: IO()
main = traceToPng sz cam testScene ".\\test.png"
	where sz  = (600, 600)
	      cam = defaultCamera (-10) (4, 4)

testScene :: Scene
testScene = [ sphere redM c r ]
	where c = Vec3 (0,0,10)
	      r = 2
	      redM = Mat (rgb 1 0 0)