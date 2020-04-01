module Main where

import Snake
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

window :: Display
window = InWindow "Haskell Snake Game" (640, 480) (100, 100)

background :: Color
background = white

render :: GameState -> Picture
render gameState = pictures $   [ fillRectangle black (16, 0) (640, 20)
                                , fillRectangle black (16, 24) (640, 20)
                                , fillRectangle black (0, 12) (20, 480)
                                , fillRectangle black (32, 12) (20, 480) ] ++
                                  fmap (convertToPicture black) snake ++ 
                                  fmap (convertToPicture blue) [food] ++
                                  gameOverPicture
    where   snake = getSnake gameState 
            food = getFood gameState
            convertToPicture :: Color -> (Int, Int) -> Picture
            convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
            fillRectangle color' (tx, ty) (w, h) =  color color' $ 
                                                    scale 1 (-1) $ 
                                                    translate (tx * 20 - 320) (ty * 20 - 240) $ 
                                                    rectangleSolid w h
            toFloat (x, y) = (fromIntegral x, fromIntegral y)
            gameOverPicture =   if (isGameOver gameState) 
                                then [  color blue $ 
                                        translate (-200) (0) $ 
                                        scale 0.5 0.5 $ 
                                        text "GAME OVER"
                                     ,  color blue $ 
                                        translate (-175) (-50) $ 
                                        scale 0.2 0.2 $ 
                                        text "Press SPACE to try again." ] 
                                else []
                                                        
update :: Float -> GameState -> GameState
update seconds gameState =  if (gameOver) 
                            then gameState
                            else GameState newSnake newFood' direction newGameOver newStdGen
    where   snake = getSnake gameState 
            food = getFood gameState
            direction = getDirection gameState
            gameOver = isGameOver gameState
            stdGen = getRandomStdGen gameState
            (wasFoodEaten, newSnake) = move food direction snake
            (newFood, newStdGen) = generateNewFood newSnake stdGen
            newFood' =  if wasFoodEaten 
                        then newFood
                        else food
            newGameOver = checkGameOver newSnake

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = changeDirection gameState LEFT
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = changeDirection gameState RIGHT 
handleKeys (EventKey (SpecialKey KeyUp   ) Down _ _) gameState = changeDirection gameState UP 
handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) gameState = changeDirection gameState DOWN 
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState =    if (isGameOver gameState)
                                                                    then initialGameState False
                                                                    else gameState
handleKeys _ gameState = gameState

main :: IO ()
main = play window background 10 (initialGameState True) render handleKeys update

