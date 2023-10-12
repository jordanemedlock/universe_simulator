module Engine.Window where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.IO.Class
import Linear
import Engine.Types
import Engine.Events
import Control.Monad
import Data.IORef
import Data.Time (getCurrentTime, diffUTCTime)
import System.Info (os)

errorCallback :: Show a => a -> [Char] -> IO ()
errorCallback err msg = do
    putStrLn "GLFW Error Occurred: "
    print err 
    putStrLn $ "with message: " ++ msg

debugCallback :: Show a => a -> IO ()
debugCallback msg = do
    print msg

initGL :: MonadIO m => V2 Int -> m ()
initGL (V2 width height) = liftIO do
    
    GL.debugOutput $= GL.Enabled
    
    when (os /= "darwin") do
        -- TODO: Figure out how to debug in macOS
        GL.debugMessageCallback $= Just debugCallback

    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.depthFunc $= Just GL.Lequal

whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing (Just x) _ = pure x
whenNothing Nothing  m = m


initWindowGL :: MonadIO m => V2 Int -> String -> m GLFW.Window
initWindowGL size title = do
    win <- initWindow size title
    initGL size
    return win

initWindow :: MonadIO m => V2 Int -> String -> m GLFW.Window
initWindow (V2 width height) title = liftIO do
    GLFW.setErrorCallback $ Just errorCallback

    initialized <- GLFW.init
    if not initialized
        then do
            errorCallback GLFW.Error'NotInitialized "Game Initialization Failed"
            error "Game Initialization Failed"
        else do
            GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
            GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True) -- This is for mac
            GLFW.windowHint (GLFW.WindowHint'Resizable False)
                
            mwindow <- GLFW.createWindow (fromIntegral width) (fromIntegral height) title Nothing Nothing
        
            GLFW.makeContextCurrent mwindow
        
            whenNothing mwindow do
                merr <- GLFW.getError 
                error $ "window failed to initialize with error: " ++ (show merr)
            

playIO :: w
       -> GLFW.Window
       -> (w -> IO ())
       -> (Event -> w -> IO w)
       -> (Double -> w -> IO w)
       -> IO ()
playIO world win draw handle step = do
    let debug = True

    worldRef <- newIORef world
    
    initEventCallbacks win handle worldRef

    prevTime <- getCurrentTime
    let loop prevTime = do
            GLFW.pollEvents

            -- GL.clearColor $= GL.Color4 0 0 0 1
            -- GL.clear [GL.ColorBuffer, GL.DepthBuffer]


            newTime <- getCurrentTime
            let delta = realToFrac $ diffUTCTime newTime prevTime
            world' <- readIORef worldRef
            world'' <- step delta world'
            draw world''
            writeIORef worldRef world''

            GLFW.swapBuffers win
            shouldClose <- GLFW.windowShouldClose win
            unless shouldClose $ loop newTime
    loop prevTime
