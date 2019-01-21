-- You're going to judge an archery contest. The
-- contestants are each going to take some number of
-- shots at a number of targets. Each target has
-- a target area and a point value. The target area
-- is a circle centered on an x,y point with a given
-- radius. Any shot that lands within the target
-- scores the value of the target in points. A shot
-- will only hit the *first* target it is found to
-- be within.
--
-- Define a function to calcuate the total score for
-- a series of shots made by a contestant, given a
-- list of targets. Use record syntax to define your
-- types, and separate your solution into as many functions
-- as you deem appropriate.
--

import Data.List (find)

data Shot = Shot { horiz :: Float
                 , vert :: Float
                 } deriving Show

data Circle = Circle { x :: Float
                     , y :: Float
                     , r :: Float
                     } deriving Show

data Target = Target { area :: Circle
                     , points :: Int
                     } deriving Show

scoreOne :: Shot -> [Target] -> Int
scoreOne Shot {horiz=sx, vert=sy} ts =
  let
    isBullseye Target {area=Circle{x=tx, y=ty, r=r}} =
      (sx-tx)^2 + (sy-ty)^2 <= r ^2
  in
    case find isBullseye ts of
      Just Target {points=p} -> p
      Nothing -> 0

scoreAll :: [Shot] -> [Target] -> Int
scoreAll ss ts = foldr scoreNext 0 ss
  where scoreNext s acc = acc + scoreOne s ts

-- This test case should give 0 points
-- scoreAll [Shot{horiz=1, vert=10}, Shot{horiz=2, vert=1}] [Target { area=Circle {x=0, y=0, r=2}, points = 9 }]
