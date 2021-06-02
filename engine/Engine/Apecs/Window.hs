module Engine.Apecs.Window where

import Prelude hiding (error)
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Apecs
import Engine.Apecs.Types
import Engine.Apecs.Logging
import Control.Monad.IO.Class
import Engine.Types
import Engine.Window
import Data.IORef

play :: (Get w IO WindowSize, Get w IO WindowTitle, Set w IO GLFW.Window, Get w IO Logger, Members w IO Logger, Has w IO EntityCounter)
     => System w ()
     -> System w ()
     -> (Event -> System w ())
     -> (Double -> System w ())
     -> System w ()
play init draw handle step = do
     w <- ask
     worldRef <- liftIO $ newIORef w
     let draw' = runSystem draw
     let handle' event = runSystem $ handle event >> ask
     let step' delta = runSystem $ step delta >> ask
     let err' err msg = do
          w <- readIORef worldRef
          w' <- runWith w do
               error $ "<" <> show err <> "> " <> msg
               ask
          writeIORef worldRef w'
     let deb' (GL.DebugMessage source typ (GL.DebugMessageID msgId) sev msg) = do
          w <- readIORef worldRef
          w' <- runWith w do
               let logFunc = case sev of
                    GL.DebugSeverityHigh -> fatal
                    GL.DebugSeverityMedium -> error
                    GL.DebugSeverityLow -> warn
                    GL.DebugSeverityNotification -> info
               logFunc $ "{" <> show msgId <> "} <" <> show typ <> "> " <> show source <> ": " <> msg
               ask
          writeIORef worldRef w'

     WindowSize screenSize <- get global
     WindowTitle windowTitle <- get global

     info "Initializing window"
     win <- initWindowWithErrors screenSize windowTitle err' deb'
     newEntity win

     init

     info "Starting game loop"
     liftIO $ playIO worldRef win draw' handle' step'


enableCursor   :: (Get w IO GLFW.Window)
               => System w ()
enableCursor = cmapM_ \win -> do
     liftIO $ GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal


disableCursor  :: (Get w IO GLFW.Window)
               => System w ()
disableCursor = cmapM_ \win -> do
     liftIO $ GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
