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

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game

unit = 40 :: Int
width  = 20 ::Int
height = 15 ::Int
toPixel :: Int -> Int
toPixel x = x * unit

block_black :: Picture
block_black = (rectangleSolid (fromIntegral (toPixel 1)) (fromIntegral (toPixel 1)))

block_grey :: Picture
block_grey = color (greyN 0.7) (rectangleSolid (fromIntegral (toPixel 1)) (fromIntegral (toPixel 1)))

-- 1/2*W - 1/2w
normalize :: Int -> Int -> Picture -> Picture
normalize x y p = 
  Translate ((0.5 * fromIntegral(width  - 1) + fromIntegral   x ) * fromIntegral (-unit))
            ((0.5 * fromIntegral(height - 1) + fromIntegral (-y)) * fromIntegral (-unit)) 
            p

initGame = GameState { 
                       tick = 0
                     , gs_status = Stopped
                     , gs_snakeStates = initState
                     , gs_food = Nothing
                     }


ticker :: Float -> GameState -> GameState
ticker tick gs@(GameState{gs_snakeStates=gs_states}) = 
        gs{gs_snakeStates = moveSnake gs_states}

eventHandler :: Event -> GameState -> GameState
eventHandler e gs = gs

renderer :: GameState -> Picture
renderer state = 
        pictures [ normalize 0 1 block_black
                 , normalize 0 2 block_grey
                 ]


data Direction = D_Up | D_Down | D_Left | D_Right deriving(Show, Eq)

data NodeType = Head | Body  deriving(Show, Eq)

data Node = Node{ nodeType::NodeType
                , node_x :: Int
                , node_y :: Int
                } deriving (Show, Eq)

initState = SnakeState { snakeMoveDir=D_Right
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
                             , snakeInnerStates :: [Node]
                             }
                            deriving(Show, Eq)

moveSnake :: SnakeState -> SnakeState
moveSnake SnakeState{ snakeMoveDir = dir, snakeInnerStates = states } = 
    SnakeState{snakeMoveDir = dir, snakeInnerStates =
        case dir of
            D_Up    -> m'   0 (-1)
            D_Down  -> m'   0   1
            D_Left  -> m' (-1)  0
            D_Right -> m'   1   0
    }
    where m = (\snakeStates dx dy -> let (x:xs) = snakeStates; l = last xs; o = takeWhile (\it -> nodeType it == Body) xs 
                                     in    o 
                                        ++ [Node {nodeType = Body, node_x = (node_x l), node_y = (node_y l)}] 
                                        ++ [Node {nodeType=Head, node_x = (node_x l) + dx, node_y = (node_y l) + dy}])
          m' = m states

-- (unfoldr (Just . uniformR (1, 6)) $ mkStdGen 137) !! 1
-- genFood :: 
-- genFood

data GameStatus = Stopped
                | Playing
                | Paused
type FoodLocation = (Int, Int)

data GameState = GameState{
    tick           :: Integer
  , gs_status      :: GameStatus
  , gs_snakeStates :: SnakeState
  , gs_food        :: Maybe FoodLocation
}

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