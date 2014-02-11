module Main where

import RayT
import RayT.Colors
import RayT.Primitives
import RayT.Vector
import RayT.Image (traceToPng)

main :: IO()
main = do
	putStrLn "tracing image..."
	traceToPng sz cam testScene "test.png"
	putStrLn "...done!"
	where sz  = (600, 600)
	      cam = Camera ((-10).*bZ + 6.*bY) screen
	      screen = Screen (3.*bY) (2.*bX) (2.*bY)

testScene :: Scene
testScene = [ sphere (colM red)   (c + 0.75.*bY) r
            , sphere (colM green) (c + 0.75.*bZ + 0.75.*bX - 0.75.*bY) r
            , sphere (colM blue)  (c + 0.75.*bZ - 0.75.*bX - 0.75.*bY) r
            , sphere (colM white) (c - 0.75.*bZ - 0.75.*bY) r
            ]
	where c        = Vec3 (0,0,10)
	      r        = 0.5
	      colM col = Mat col