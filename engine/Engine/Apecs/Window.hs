module Engine.Apecs.Window where


import qualified Graphics.UI.GLFW as GLFW
import Apecs
import Control.Monad.IO.Class
import Engine.Types
import Engine.Window

play :: GLFW.Window
     -> System w ()
     -> (Event -> System w ())
     -> (Double -> System w ())
     -> System w ()
play win draw handle step = do
    w <- ask
    let draw' = runSystem draw
    let handle' event = runSystem $ handle event >> ask
    let step' delta = runSystem $ step delta >> ask
    liftIO $ playIO w win draw' handle' step'