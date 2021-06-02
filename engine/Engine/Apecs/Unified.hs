module Engine.Apecs.Unified where

import Engine.Apecs.Types
import Engine.Types
import Apecs

import qualified Graphics.UI.GLFW as GLFW


ifPaused f = get global >>= \case
    Paused -> f
    _ -> return ()
ifPlaying f = get global >>= \case
    Playing -> f   
    _ -> return () 

handleGameState event onUpdated = do
    ifPaused $ case event of
        (KeyEvent GLFW.Key'Escape _ GLFW.KeyState'Released ms) -> set global Playing >> onUpdated (Paused, Playing)
        _ -> return ()
    ifPlaying $ case event of 
        (KeyEvent GLFW.Key'Escape _ GLFW.KeyState'Released ms) -> set global Paused >> onUpdated (Playing, Paused)
        _ -> return ()

