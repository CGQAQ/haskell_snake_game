import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Src.Core (
        toPixel
      , width
      , height
      , GameState

      , initGame

      , ticker
      , eventHandler
      , renderer
    )
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
main :: IO ()
main = 
  let display_ = (InWindow
                  "Hello World"                       -- window title
                  (toPixel width, toPixel height)     -- window size
                  (10, 10)                            -- window position
                 )
  in
    play display_ white 1 initGame renderer eventHandler ticker
