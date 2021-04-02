module Engine.Events where

-- import Apecs
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import Engine.Types


whenReleased :: Monad m => GLFW.KeyState -> m () -> m ()
whenReleased ks system 
        | ks `elem` ([GLFW.KeyState'Released, GLFW.KeyState'Repeating] :: [GLFW.KeyState]) = system
        | otherwise = return ()

ifHeld :: Monad m => GLFW.KeyState -> m () -> m () -> m ()
ifHeld ks held released
        | ks `elem` ([GLFW.KeyState'Pressed, GLFW.KeyState'Repeating] :: [GLFW.KeyState]) = held
        | otherwise = released

initEventCallbacks :: GLFW.Window -> (Event -> w -> IO w) -> IORef w -> IO ()
initEventCallbacks win handle worldRef = do
    GLFW.setKeyCallback win $ Just $ \w k i ks ms -> do
        world' <- readIORef worldRef
        world'' <- handle (KeyEvent k i ks ms) world'
        writeIORef worldRef world''
    GLFW.setCursorPosCallback win $ Just $ \w x y -> do
        world' <- readIORef worldRef
        world'' <- handle (CursorEvent x y) world'
        writeIORef worldRef world''
    GLFW.setCharCallback win $ Just $ \w c -> do
        world' <- readIORef worldRef
        world'' <- handle (CharEvent c) world'
        writeIORef worldRef world''