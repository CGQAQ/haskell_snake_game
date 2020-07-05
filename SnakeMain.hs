import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Src.Core (
        width
      , height
      , GameState
    )

unit = 40 :: Int

main :: IO ()
main = do
        display_ <- display
                        (InWindow
                        "Hello World"     -- window title
                                (width * unit, height * unit)       -- window size
                                (10, 10))        -- window position
                        green                    -- background color
                        picture                  -- picture to display
        print "hello"
        {-
                play    :: Display              -- ^ Display mode.
                -> Color                -- ^ Background color.
                -> Int                  -- ^ Number of simulation steps to take for each second of real time.
                -> world                -- ^ The initial world.
                -> (world -> Picture)   -- ^ A function to convert the world a picture.
                -> (Event -> world -> world)
                        -- ^ A function to handle input events.
                -> (Float -> world -> world)
                        -- ^ A function to step the world one iteration.
                        --   It is passed the period of time (in seconds) needing to be advanced.
                -> IO ()
        -}
        -- play $ display_ $ white $ 0 $ GameState{} $ renderer $ eventHandler $ ticker

ticker :: Float -> GameState -> GameState
ticker tick gs = 
        undefined

eventHandler :: Event -> GameState -> GameState
eventHandler = undefined

renderer :: GameState -> Picture
renderer state = 
        picture

picture :: Picture
picture
        = Translate (-170) (-20) -- shift the text to the middle of the window
        $ Scale 0.5 0.5          -- display it half the original size
        $ Text "Hello World"     -- text to display