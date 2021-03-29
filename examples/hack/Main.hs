{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Time (getCurrentTime, diffUTCTime)
import Engine
import Engine.Apecs
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Linear
import Data.IORef
import Control.Monad
import Apecs hiding (($=), asks, ask)
import qualified Apecs
import Apecs.TH
import Apecs.Components (Filter(..))
import System.Exit
import Control.Lens
import Data.Maybe (fromMaybe)


data Orbit = Orbit { rotation :: Float
                   , revolution :: Float
                   , rotationSpeed :: Float 
                   , revolutionSpeed :: Float 
                   , center :: V3 Float
                   , radius :: Float
                   } deriving Show

data Hud = Hud deriving Show

data Earth = Earth deriving Show
instance Component Earth where type Storage Earth = Unique Earth

data Moon = Moon deriving Show
instance Component Moon where type Storage Moon = Unique Moon

data GameState = Paused | Playing | Closed | Terminal deriving (Show, Eq, Enum)
instance Semigroup GameState where (<>) = const
instance Monoid GameState where mempty = Playing
instance Component GameState where type Storage GameState = Global GameState

data CursorPos = CursorPos Double Double deriving (Show, Eq)
instance Semigroup CursorPos where (CursorPos x1 y1) <> (CursorPos x2 y2) = CursorPos (x1+x2) (y1+y2)
instance Monoid CursorPos where mempty = CursorPos 0 0
instance Component CursorPos where type Storage CursorPos = Global CursorPos

data GameSettings = GameSettings { cameraSpeed :: (Float, Float) -- (cursor, keyboard)
                                 , screenSize :: V2 Int
                                 , debug :: Bool
                                 } deriving Show
instance Semigroup GameSettings where (<>) = const
instance Monoid GameSettings where mempty = GameSettings (0.1, 0.01) (V2 1920 1080) True
instance Component GameSettings where type Storage GameSettings = Global GameSettings

#define MAP_COMPS [ ''Pos, ''Rot, ''Texture, ''Shader, ''Orbit, ''Mesh, ''CamRot, ''Pos2D \
                  , ''Scale, ''Font, ''TextBox, ''Hud, ''Console, ''Command, ''FormControl \
                  , ''TextInput, ''Size2D, ''Hidden, ''Focus \
                  ]
#define NON_MAP_COMPS [ ''Earth, ''Moon, ''Camera, ''GameState, ''GameSettings, ''CursorPos \
                      , ''TerminalInput \
                      ]

makeMapComponents MAP_COMPS
makeWorld "World" $ MAP_COMPS <> NON_MAP_COMPS

debugLog msg = do
    settings <- get global
    when (debug settings) $ liftIO $ putStrLn msg

main :: IO ()
main = do
    let debug = True
    when debug $ putStrLn "Initiating world"
    world <- initWorld

    -- GL.polygonMode $= (GL.Line, GL.Line)

    runWith world $ do

        settings <- get global

        debugLog "Initiating window"
        window <- initWindow (screenSize settings) "Camera movement example"

        debugLog "Initiating systems"
        initialize

        debugLog "playing game"
        play window draw handle step

    GLFW.terminate


initialize :: System World ()
initialize = do
    (earthTex, _) <- loadTexture "resources/textures/8k_earth_daymap.png"
    (moonTex, _) <- loadTexture "resources/textures/8k_moon.png"
    program <- either error id <$> loadShader "resources/shaders/lighting"
    glyph <- either error id <$> loadShader "resources/shaders/glyph"
    font <- loadFont "resources/fonts/Inconsolata.ttf" 48 glyph

    (V2 w h) <- (realToFrac <$>) . screenSize <$> get global

    withShader glyph do
        "projection" $== ortho 0 w h 0 (-1) (1 :: Float)


    sphereMesh <- createSphereAsset 20

    cameraEty <- newEntity (Camera, Pos $ V3 2 2 2, CamRot 0 0)
    earthEty <- newEntity ( Earth
                        , Pos $ V3 0 0 0, Rot $ Left $ V3 (-90) 0 0, Scale $ V3 1 1 1
                        , Texture earthTex, Shader program, Mesh sphereMesh
                        , Orbit 0 0 0.1 0 (V3 0 0 0) 0
                        )
    moonEty <- newEntity  ( Moon
                        , Pos $ V3 10 0 0, Rot $ Left $ V3 0 0 0, Scale $ V3 0.1 0.1 0.1
                        , Texture moonTex, Shader program, Mesh sphereMesh
                        , Orbit 0 0 0.1 0.1 (V3 0 0 0) 10
                        )
    fontEty <- newEntity font
    hudEty <- newEntity ( Hud, TextBox "Hello World!" fontEty (V4 1 1 1 1) 0.5, Pos2D $ V2 10 30 )
    textInputEty <- newEntity ( TerminalInput
                              , Console, FormControl, TextInput 0
                              , TextBox "" fontEty (V4 1 1 1 1) 0.5
                              , Pos2D $ V2 40 (h - 60), Size2D $ V2 (w - 80) 30
                              , Command Nothing
                              )

    return ()

draw = do
    gameState <- get global
    case gameState of 
        Terminal -> drawTerminal
        _ -> drawObjects (Filter @Mesh) >> drawTextBoxes (Hud)

drawObjects :: forall tag. (Get World IO tag, Members World IO tag) => tag -> System World ()
drawObjects _ = cmapM_ $ \(Camera, Pos camPos, CamRot pitch yaw) -> do
    cmapM_ $ \(_ :: tag, Texture tex, Shader sdr, Mesh mesh, Pos pos, Rot erot, mscale) -> do
        let view = lookAt camPos (camPos + cameraForward pitch yaw) (V3 0 1 0)
        let proj = perspective (70 :: Float) (16.0/9) 0.1 100.0
        let (Scale size) = fromMaybe (Scale $ V3 1 1 1) mscale
        let scaleMat = scaled $ point size
        let rotMat = either rotationMatrix fromQuaternion erot
        let model = mkTransformationMat rotMat pos !*! scaleMat


        withShader sdr do
            "model"         $== model
            "view"          $== view
            "projection"    $== proj

            "objectColor"   $== (V4 1.0 0.5 0.3 1.0 :: V4 Float)
            "ambientColor"  $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)
            "viewPos"       $== camPos
            "lightPos"      $== (V3 1.1 1.0 2.0 :: V3 Float)
            "lightColor"    $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)

            liftIO $ GL.activeTexture $= GL.TextureUnit 0
            liftIO $ GL.textureBinding GL.Texture2D $= Just tex

            drawMeshAsset mesh -- size of the cube array


moveCamera :: (Float -> Float -> V3 Float) -> Float -> System World ()
moveCamera dir vel = cmap \(Camera, Pos pos, CamRot pitch yaw, GameSettings {cameraSpeed=(_, speed)}) ->
    Pos $ pos + dir pitch yaw ^* vel ^* speed



handle :: Event -> System World ()
handle event = do
    textInput event
    Apecs.get global >>= \case 
        Playing -> playingInput event
        Terminal -> terminalInput event (Apecs.set global Playing)
        _ -> return ()


playingInput :: Event -> System World ()
playingInput (KeyEvent GLFW.Key'Slash i ks mods) = Apecs.set global Terminal >> initTerminal
playingInput (KeyEvent GLFW.Key'Escape i GLFW.KeyState'Released mods) = liftIO exitSuccess
playingInput (KeyEvent GLFW.Key'W i ks mods) = moveCamera cameraForward (1)
playingInput (KeyEvent GLFW.Key'A i ks mods) = moveCamera cameraRight (-1)
playingInput (KeyEvent GLFW.Key'S i ks mods) = moveCamera cameraForward (-1)
playingInput (KeyEvent GLFW.Key'D i ks mods) = moveCamera cameraRight (1)
playingInput (KeyEvent GLFW.Key'Q i ks mods) = moveCamera cameraUp (1)
playingInput (KeyEvent GLFW.Key'E i ks mods) = moveCamera cameraUp (-1)
playingInput (CursorEvent x y) = do
    (CursorPos px py, GameSettings {cameraSpeed=(speed, _)}) <- get global

    let dx = realToFrac (x - px) * speed
    let dy = realToFrac (py - y) * speed
    cmap \(Camera, CamRot pitch yaw) -> (Camera, CamRot (clamp (-89) 89 (pitch + dy)) (yaw + dx), CursorPos x y)
playingInput _ = do
    return ()

runOrbit :: Double -> System World ()
runOrbit delta = cmap inner
    where 
        inner (Orbit rot rev rotS revS c r, Pos _, Rot _) = ( Orbit rot' rev' rotS revS c r, Pos pos', Rot (Right rotM))
            where
                rot' = rot + rotS * realToFrac delta
                rev' = rev + revS * realToFrac delta
                x = r * sin rev'
                z = r * cos rev'
                pos' = V3 x 0 z
                rotM = axisAngle (V3 0 1 0) rot'

runConsole :: Double -> System World ()
runConsole delta = return ()

step :: Double -> System World ()
step delta = do
    gameState <- get global
    case gameState of
        Playing -> runOrbit delta
        Terminal -> runConsole delta
        _ -> return ()



clamp :: (Ord a) => a -> a -> a -> a
clamp min max x | x < min = min
                | x > max = max
                | otherwise = x
