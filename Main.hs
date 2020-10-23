module Main where

import System.IO
import Data.List
import Control.Monad
import Graphics.UI.GLUT

scale'::GLfloat
scale' = 0.25

points::[(GLfloat, GLfloat)]
points= concat [[(scale' * x, scale' * y) | x <- [-4..4]] | y <- [-4..4]]


main :: IO ()
main = do
   (_proName, _args) <- getArgsAndInitialize
   _window <- createWindow "TEST"
   displayCallback $= (\x -> clear [ColorBuffer] >> display x) points
   mainLoop

display ::[(GLfloat, GLfloat)] -> DisplayCallback
display xs = do
   let c3 = (\(x, y, z) -> color $ Color3 (x::GLfloat) (y::GLfloat) (z::GLfloat))
   let v2 = (\(x, y) -> Vertex2 (x::GLfloat) (y::GLfloat))
   if xs == []
     then flush
     else
        if length xs `mod` 2 == 0
           then do
              c3 (0.3,0.3,0.3)
              rect (v2 $ head xs) (v2 $ get_diagonal $ head xs)
              display (tail xs)
            else do
               c3 (1,0,0)
               rect (v2 $ head xs) (v2 $ get_diagonal $ head xs)
               display (tail xs)
    where
       get_diagonal :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) 
       get_diagonal (x, y)
          | (x,y) == (-1, -1)  = (-1, -1)
          | (x,y) == (-1, y)   = (-1, y)
          | (x,y) == (x,- 1)   = (x, -1)
          | otherwise          = (x - 0.25, y -0.25)
