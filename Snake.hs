module Snake where

import Data.Map as Map
import System.Random

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)
type Food = (Int, Int)
type Snake = [Food]

cols = 32
rows = 24

directionVectorMap = Map.fromList $ zip [UP, DOWN, LEFT, RIGHT] 
                                        [(0, (-1)), (0, 1), ((-1), 0), (1, 0)]

move :: Food -> Direction -> Snake -> (Bool, Snake)
move food direction snake = if wasFoodEaten 
                            then (True, newHead : snake)
                            else (False, newHead : init snake)
    where   wasFoodEaten = newHead == food
            newHead = directionVectorMap ! direction +: head snake
            (a, b) +: (c, d) = (a + c, b + d)

checkGameOver :: Snake -> Bool
checkGameOver snake =   headX == 0 || headX == cols || 
                        headY == 0 || headY == rows ||
                        head' `elem` tail'
    where   head' = head snake
            (headX, headY) = head'
            tail' = tail snake

generateNewFood :: Snake -> StdGen -> (Food, StdGen)
generateNewFood snake stdGen =  if newFood `elem` snake
                                then generateNewFood snake stdGen3
                                else ((foodX, foodY), stdGen3)
        where   (foodX, stdGen2) = randomR (1, 31) stdGen
                (foodY, stdGen3) = randomR (1, 23) stdGen2
                newFood = (foodX, foodY)

data GameState = GameState      { getSnake :: Snake
                                , getFood :: Food
                                , getDirection :: Direction
                                , isGameOver :: Bool
                                , getRandomStdGen :: StdGen }

changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState s f d g r) newDir = GameState s f newDir g r 

initialGameState gameOver = GameState   { getSnake = [  (snakeX, snakeY), 
                                                        (snakeX, snakeY - 1), 
                                                        (snakeX, snakeY - 2), 
                                                        (snakeX - 1, snakeY - 2), 
                                                        (snakeX - 2, snakeY - 2)]
                                        , getFood = (3, 3)
                                        , getDirection = DOWN
                                        , isGameOver = gameOver
                                        , getRandomStdGen = mkStdGen 100 }
        where   snakeX = cols `div` 2
                snakeY = rows `div` 2
