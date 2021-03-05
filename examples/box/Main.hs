
module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import Engine
import Text.RawString.QQ
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Linear
import Control.Lens


data GameState = GameState 
    { shader :: GL.Program
    , cube :: MeshAsset
    }

windowSize :: V2 Int
windowSize = V2 1920 1080

main :: IO ()
main = do
    window <- initWindow (V2 1920 1080) "Box example"

    Right program <- loadShader "resources/shaders/simple" 

    let ratio = windowSize^._x.to fromIntegral / windowSize^._y.to fromIntegral :: Float

    projection <- perspectiveMatrix 45 ratio 0.1 100.0

    withShader program $ "projection" $== projection

    cube <- createCubeAsset

    startTime <- getCurrentTime

    mainLoop window startTime (GameState program cube)

    GLFW.terminate

mainLoop :: GLFW.Window -> UTCTime -> GameState -> IO ()
mainLoop window previousTime (GameState program cube) = do
    GLFW.pollEvents

    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    thisTime <- getCurrentTime
    let deltaTime = diffUTCTime thisTime previousTime
    let dt = (realToFrac deltaTime :: Double)

    model <- eye4 :: IO (GL.GLmatrix Float)
    
    view <- lookAtMatrix (GL.Vector3 1 1 1) (GL.Vector3 0 0 0) (GL.Vector3 0 1 0) :: IO (GL.GLmatrix Float)

    withShader program do
        "model"         $== model
        "view"          $== view

        "objectColor"   $== (GL.Color4 1.0 0.5 0.3 1.0 :: GL.Color4 Float)
        "ambientColor"  $== (GL.Color4 1.0 1.0 1.0 1.0 :: GL.Color4 Float)
        "viewPos"       $== (GL.Vector3 1 1 1 :: GL.Vector3 Float)
        "lightPos"      $== (GL.Vector3 1.1 1.0 2.0 :: GL.Vector3 Float)
        "lightColor"    $== (GL.Color4 1.0 1.0 1.0 1.0 :: GL.Color4 Float)

        drawMeshAsset cube
        

    GLFW.swapBuffers window
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose 
        then return ()
        else mainLoop window thisTime (GameState program cube)

