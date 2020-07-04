module Core where

data Direction = Up | Down | Left | Right deriving(Show, Eq)

data NodeType = Head | Body  deriving(Show, Eq)

data Node = Node{ nodeType::NodeType
                , x :: Int
                , y :: Int
                } deriving (Show, Eq)

initState = [ Node { nodeType=Body
                   , x = 0
                   , y = 0
                   }
            , Node { nodeType=Body
                   , x = 1
                   , y = 0
                   }
            , Node { nodeType=Body
                   , x = 2
                   , y = 0
                   }
            , Node { nodeType=Head
                   , x = 3
                   , y = 0
                   }
            ]

data SnakeState = SnakeState { snakeMoveDir :: Direction
                             , snakeInnerStates :: [Node]
                             }
                            deriving(Show, Eq)

moveSnake :: SnakeState -> SnakeState
moveSnake SnakeState{ snakeMoveDir = dir, snakeInnerStates = states } = 
    undefined