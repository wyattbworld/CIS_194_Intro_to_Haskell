{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-\}
--Author C. Wyatt Bruchhauser
--Date Aug 18, 2026
--Title: Homework 1 CIS 194 Introduction to Haskell (Fall 2016)

import CodeWorld

main :: IO ()
main = exercise3

-- Exercise 1

botCircle, midCircle, topCircle :: Bool -> Picture
botCircle True  = colored green (translated 0 (-3) (solidCircle 1))
botCircle False = colored black (translated 0 (-3) (solidCircle 1))
midCircle True  = colored orange (translated 0 0 (solidCircle 1))
midCircle False = colored black (translated 0 0 (solidCircle 1))
topCircle True  = colored red (translated 0 3  (solidCircle 1))
topCircle False = colored black (translated 0 3  (solidCircle 1))

frame :: Picture
frame = rectangle 4 9

trafficSignals :: Int -> Picture
trafficSignals t
  | (t `mod` 4) == 0 = botCircle False & midCircle False & topCircle True --red light
  | (t `mod` 4) == 1 = botCircle False & midCircle True & topCircle False --yellow light
  | (t `mod` 4) == 2 = botCircle False & midCircle True & topCircle True --red & yellow light
  | otherwise        = botCircle True & midCircle False & topCircle False --green light

trafficLight t = trafficSignals t & frame

trafficController :: Double -> Picture

trafficCounter :: Double -> Int
trafficCounter t = round (t) `mod` 12

trafficController t
  | trafficCounter t >= 0 && trafficCounter t < 5  = trafficLight 3
  | trafficCounter t == 5                          = trafficLight 1
  | trafficCounter t >= 6 && trafficCounter t < 11 = trafficLight 0
  | otherwise                                      = trafficLight 2

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Integer -> Double -> Picture
tree 0 r = colored pink (solidCircle (minimum [r, 1]))
tree n r = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) r) & rotated (- pi/10) (tree (n-1) r))
  
bloomingAnimation :: Double -> Picture
bloomingAnimation t = tree 8 (t/10)

exercise2 :: IO ()
exercise2 = animationOf bloomingAnimation

-- Exercise 3

wall, ground, storage, box :: Picture
wall    = colored (duller 0.3 red) (solidRectangle 1 1)
ground  = colored (duller 0.3 grey) (solidRectangle 1 1)
storage = colored black (solidCircle 0.25) & ground
box     = colored brown (solidRectangle 0.8 0.8) & ground

drawTile :: Integer -> Picture
drawTile i
  | i == 1 = wall
  | i == 2 = ground
  | i == 3 = storage
  | i == 4 = box
  | otherwise = blank

pictureOfTileAtIndex :: Integer -> Integer -> Picture
pictureOfTileAtIndex i j = translated (fromIntegral i) (fromIntegral j) (drawTile (maze i j))

pictureOfRow :: Integer -> Picture
pictureOfRow j = pictures (map (\\i -> pictureOfTileAtIndex i j) [-10..10])

pictureOfMaze :: Picture
pictureOfMaze = pictures (map (pictureOfRow) [-10..10])

exercise3 :: IO ()
exercise3 = drawingOf (pictureOfMaze)

maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 }
