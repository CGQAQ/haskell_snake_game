{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS -Wall #-} 

module Src.Core (
    toPixel
  , width
  , height
  , GameState

  , initGame

  , ticker
  , eventHandler
  , renderer
) where

import Debug.Trace

import Data.List
-- import Data.Time.Clock.POSIX (getPOSIXTime)

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game

import System.Random

unit = 40 :: Int
width  = 20 ::Int
height = 15 ::Int
toPixel :: Int -> Int
toPixel x = x * unit

block_black :: Picture
block_black = (rectangleSolid (fromIntegral (toPixel 1)) (fromIntegral (toPixel 1)))

block_grey :: Picture
block_grey = color (greyN 0.5) (rectangleSolid (fromIntegral (toPixel 1)) (fromIntegral (toPixel 1)))

block_food :: Picture
block_food = color (greyN 0.3) (rectangleSolid (fromIntegral (toPixel 1)) (fromIntegral (toPixel 1)))

-- 1/2*W - 1/2w
normalize :: Int -> Int -> Picture -> Picture
normalize x y p = 
  Translate ((0.5 * fromIntegral(width  - 1) + fromIntegral (-x) ) * fromIntegral (-unit))
            ((0.5 * fromIntegral(height - 1) + fromIntegral (-y)) * fromIntegral (-unit)) 
            p

initGame = GameState { tick = 0
                     , gs_score = 0
                     , gs_status = Stopped
                     , gs_snakeStates = initState
                     , gs_food = Nothing
                     }


ticker :: Float -> GameState -> GameState
{-
    gs_score       :: Int
  , gs_needGrow    :: Bool
  , gs_snakeStates :: SnakeState
  , gs_food        :: Maybe FoodLocation
-}
ticker _ gs@( GameState tick sc status snake food ) 
    | head_x < 0 || head_x > width - 1  = reset
    | head_y < 0 || head_y > height - 1 = reset
    | otherwise = 
        gs { tick = tick'
           , gs_score = score'
           , gs_status = status
           , gs_snakeStates = snake'
           , gs_food = food'
           }
        where tick'  = tick + 1 
              snake' = case status of
                             Playing -> case food of
                                        -- otherwise -> traceShowId $ moveSnake snake{snakeNeedGrow=True}
                                        Nothing -> moveSnake snake{snakeNeedGrow=True}
                                        _ -> moveSnake snake
                             _ -> snake
              score' = case status of
                             Playing -> case food of
                                        -- otherwise -> traceShowId $ moveSnake snake{snakeNeedGrow=True}
                                        Nothing -> sc + 1
                                        _ -> sc
                             _ -> sc
              head_x = node_x $ last $ snakeInnerStates snake'
              head_y = node_y $ last $ snakeInnerStates snake'
              food'  = case food of
                       Just x  -> if
                                  | x == (head_x, head_y)  -> Nothing
                                  | otherwise -> gs_food gs
                       Nothing -> Just $ genFood tick

              reset  = gs{gs_status = Stopped, gs_snakeStates = initState, gs_food=Nothing, gs_score=0}

        --  status == Playing = gs{gs_snakeStates = moveSnake snake}
        --  otherwise = gs

eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey key Down modkey (x, y)) gs = 
  -- case trace "key pushed" key of
  case key of
    SpecialKey s -> case s of
                    {- move around -}
                    KeyLeft 
                      | dir == D_Right -> gs
                      | otherwise -> gs{gs_snakeStates = snakeState{snakeMoveDir=D_Left}}
                    KeyRight 
                      | dir == D_Left -> gs
                      | otherwise -> gs{gs_snakeStates = snakeState{snakeMoveDir=D_Right}}
                    KeyUp 
                      | dir == D_Down -> gs
                      | otherwise -> gs{gs_snakeStates = snakeState{snakeMoveDir=D_Up}}
                    KeyDown 
                      | dir == D_Up -> gs
                      | otherwise -> gs{gs_snakeStates = snakeState{snakeMoveDir=D_Down}}

                    {- game status control -}
                    -- start/pause game
                    KeyHome
                      | status == Stopped || status == Paused -> gs{gs_status = Playing}
                      | status == Playing -> gs{gs_status = Paused}
                      | otherwise -> gs
                    -- reset game
                    KeyEnd -> gs{gs_status = Stopped, gs_snakeStates = initState, gs_food=Nothing, gs_score=0}
                    _ -> gs
    _ -> gs
  where status     = gs_status gs
        snakeState = gs_snakeStates gs
        dir        = snakeMoveDir snakeState
eventHandler _ gs = gs

renderer :: GameState -> Picture
renderer state = 
        -- traceShow (gs_snakeStates state) $ 
          pictures $ 
                    [Translate 
                          (-0.5 * fromIntegral (toPixel width)) 
                          (0.5 * fromIntegral (toPixel height) - 22) $
                          Scale 0.3 0.2 $
                          Text $ "Frames: " ++ (show $ tick state) ++ " Score: " ++ (show $ gs_score state)
                    , food'] 
                  ++ map (
            \n@Node{node_x=x, node_y=y} -> 
                  let t = nodeType n in if
                  | t == Head -> normalize x y block_black
                  | t == Body -> normalize x y block_grey
            ) snake
        where snake = snakeInnerStates $ gs_snakeStates state
              food  = gs_food state
              food' = case food of
                           Just (x, y) -> normalize x y block_food
                           Nothing     -> Blank
        -- normalize (-1) 0 block_black


data Direction = D_Up | D_Down | D_Left | D_Right deriving(Show, Eq)

data NodeType = Head | Body  deriving(Show, Eq)

data Node = Node{ nodeType::NodeType
                , node_x :: Int
                , node_y :: Int
                } deriving (Show, Eq)

initState = SnakeState { snakeMoveDir=D_Right
                       , snakeNeedGrow=False
                       , snakeInnerStates = 
                           [ Node { nodeType=Body
                           , node_x = 0
                           , node_y = 0
                           }
                           , Node { nodeType=Body
                           , node_x = 1
                           , node_y = 0
                           }
                           , Node { nodeType=Body
                           , node_x = 2
                           , node_y = 0
                           }
                           , Node { nodeType=Head
                           , node_x = 3
                           , node_y = 0
                           }
                           ]
                       }

data SnakeState = SnakeState { snakeMoveDir :: Direction
                             , snakeNeedGrow::Bool
                             , snakeInnerStates :: [Node]
                             }
                            deriving(Show, Eq)

moveSnake :: SnakeState -> SnakeState
moveSnake SnakeState{ snakeMoveDir = dir, snakeInnerStates = states, snakeNeedGrow = ng} = 
    SnakeState { snakeMoveDir = dir
               , snakeInnerStates =
                    case dir of
                    D_Up    -> m'   0   1
                    D_Down  -> m'   0 (-1)
                    D_Left  -> m' (-1)  0
                    D_Right -> m'   1   0
               , snakeNeedGrow = False
    }
    where m = (\snakeStates dx dy -> let (x:xs) = snakeStates; l = last xs; o = takeWhile (\it -> nodeType it == Body) xs 
                                     in
                                        if  
                                          |ng==False -> 
                                                o 
                                            ++ [Node {nodeType = Body, node_x = (node_x l), node_y = (node_y l)}] 
                                            ++ [Node {nodeType=Head, node_x = (node_x l) + dx, node_y = (node_y l) + dy}]
                                          |ng==True -> 
                                                  [x]
                                              ++  o
                                              ++ [Node {nodeType = Body, node_x = (node_x l), node_y = (node_y l)}] 
                                              ++ [Node {nodeType=Head, node_x = (node_x l) + dx, node_y = (node_y l) + dy}])
                                      
          m' = m states

-- (unfoldr (Just . uniformR (1, 6)) $ mkStdGen 137) !! 1
genFood :: Int -> FoodLocation
genFood t = 
  (xs, ys)
  where xs = (unfoldr (Just . uniformR (0, 19)) $ mkStdGen (137+t)) !! t :: Int
        ys = (unfoldr (Just . uniformR (0, 14)) $ mkStdGen (137+t)) !! t :: Int

data GameStatus = Stopped
                | Playing
                | Paused
                deriving (Show, Eq)
type FoodLocation = (Int, Int) 

data GameState = GameState{
    tick           :: Int
  , gs_score       :: Int
  -- , gs_needFood    :: Bool -- Just look at gs_food if its Nothing
  -- , gs_needGrow    :: Bool -- Moved to SnakeState
  , gs_status      :: GameStatus
  , gs_snakeStates :: SnakeState
  , gs_food        :: Maybe FoodLocation
} deriving (Show)

-- ---------------------------------------------------------------------------------------------
-- testMoveSnake :: Test
-- testMoveSnake = 
--     TestCase $ assertEqual $ "right: " SnakeState{snakeMoveDir=D_Right, snakeInnerStates=[
--             Node { nodeType=Body
--                    , node_x = 1
--                    , node_y = 0
--                    }
--             , Node { nodeType=Body
--                    , node_x = 2
--                    , node_y = 0
--                    }
--             , Node { nodeType=Body
--                    , node_x = 3
--                    , node_y = 0
--                    }
--             , Node { nodeType=Head
--                    , node_x = 4
--                    , node_y = 0
--                    }]}
--             moveSnake SnakeState{snakeMoveDir=D_Right, snakeInnerStates=initState}