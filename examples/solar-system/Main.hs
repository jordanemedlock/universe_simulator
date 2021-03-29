{-# LANGUAGE TemplateHaskell       #-}

module Main where

import Engine
import Engine.Apecs
import Types
import Apecs hiding (($=))
import Apecs.TH
import Apecs.Components
import Linear
import Control.Monad
import Data.Maybe
import Graphics.UI.GLFW (Key(..), KeyState(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import System.Exit

makeMapComponents $ defaultMapComponents <> mapComponents
makeWorld "World" $ defaultAllComponents <> mapComponents <> nonMapComponents

-- type RandIO = RandT StdGen IO
type WorldSystem a = SystemT World IO a -- I want dem monads!!!!

main = do
    world <- initWorld

    runWith world $ do

        settings <- get global

        debugLog "Initiating window"
        window <- initWindow (screenSize settings) "Solar System"

        liftIO $ GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
        -- liftIO $ GL.polygonMode $= (GL.Line, GL.Line)


        debugLog "Initiating system"
        initialize

        debugLog "Playing game"
        play window draw handle step

debugLog msg = do
    settings <- get global
    when (debug settings) $ liftIO $ putStrLn msg

initialize :: WorldSystem ()
initialize = do
    (moonTex, _) <- loadTexture "resources/textures/8k_moon.png"
    program <- either error id <$> loadShader "resources/shaders/lighting"
    fixedLighting <- either error id <$> loadShader "resources/shaders/fixed_lighting"
    glyph <- either error id <$> loadShader "resources/shaders/glyph"
    font <- loadFont "resources/fonts/Inconsolata.ttf" 48 glyph
    (selectTex, _) <- loadTexture "resources/textures/select_circle.png"
    unitTex <- unitTexture $ V4 1 1 1 1

    (V2 w h) <- (realToFrac <$>) . screenSize <$> get global

    withShader glyph do
        "projection" $== ortho 0 w h 0 (-1) (1 :: Float)


    sphereMesh <- createSphereAsset 20
    planeMesh <- createPlaneAsset
    xLine <- createLinesAsset [V3 0 0 0, V3 1 0 0]
    yLine <- createLinesAsset [V3 0 0 0, V3 0 1 0]
    zLine <- createLinesAsset [V3 0 0 0, V3 0 0 1]

    cameraEty <- newEntity (Camera, Pos $ V3 0 0 10, CamRot 0 0, Vel $ V3 0 0 0)

    Apecs.set global (gigams - 1) -- 10^8

    _ <- newEntity (Mesh xLine, Color $ V4 1 0 0 1, Pos $ V3 0 0 0, Shader fixedLighting, Texture unitTex)
    _ <- newEntity (Mesh yLine, Color $ V4 0 1 0 1, Pos $ V3 0 0 0, Shader fixedLighting, Texture unitTex)
    _ <- newEntity (Mesh zLine, Color $ V4 0 0 1 1, Pos $ V3 0 0 0, Shader fixedLighting, Texture unitTex)
    

    (sunTex, _) <- loadTexture "resources/textures/8k_sun.png"
    sunEty <- newEntity ( Sun
                        , Pos $ V3 0 0 0, RotEuler $ V3 (-90) 0 0, Scale $ V3 7.0 7.0 7.0 -- scale is radius for spheres
                        , Texture sunTex, Shader fixedLighting, Mesh sphereMesh
                        )

                        
    (mercuryTex, _) <- loadTexture "resources/textures/8k_mercury.png"
    mercuryEty <- newEntity ( Planet
                          , Pos $ V3 460 0 0, RotEuler $ V3 (-90) 0 0, Scale $ V3 0.024 0.024 0.024
                          , Texture mercuryTex, Shader program, Mesh sphereMesh
                          )
                        
    (venusTex, _) <- loadTexture "resources/textures/8k_venus_surface.png"
    venusEty <- newEntity ( Planet
                          , Pos $ V3 110 0 0, RotEuler $ V3 (-90) 0 0, Scale $ V3 0.061 0.061 0.061
                          , Texture venusTex, Shader program, Mesh sphereMesh
                          )
                        
    (earthTex, _) <- loadTexture "resources/textures/8k_earth_daymap.png"
    earthEty <- newEntity ( Planet
                          , Pos $ V3 147 0 0, RotEuler $ V3 (-90) 0 0, Scale $ V3 0.064 0.064 0.064
                          , Texture earthTex, Shader program, Mesh sphereMesh
                          )

    (marsTex, _) <- loadTexture "resources/textures/8k_mars.png"
    marsEty <- newEntity ( Planet
                          , Pos $ V3 206 0 0, RotEuler $ V3 (-90) 0 0, Scale $ V3 0.034 0.034 0.034
                          , Texture marsTex, Shader program, Mesh sphereMesh
                          )


    (jupiterTex, _) <- loadTexture "resources/textures/8k_jupiter.png"
    jupiterEty <- newEntity ( Planet
                          , Pos $ V3 7400 0 0, RotEuler $ V3 (-90) 0 0, Scale $ V3 0.70 0.70 0.70
                          , Texture jupiterTex, Shader program, Mesh sphereMesh
                          )


    (saturnTex, _) <- loadTexture "resources/textures/8k_saturn.png"
    saturnEty <- newEntity ( Planet
                          , Pos $ V3 14000 0 0, RotEuler $ V3 (-90) 0 0, Scale $ V3 0.58 0.58 0.58
                          , Texture saturnTex, Shader program, Mesh sphereMesh
                          )


    (uranusTex, _) <- loadTexture "resources/textures/2k_uranus.png"
    uranusEty <- newEntity ( Planet
                          , Pos $ V3 27000 0 0, RotEuler $ V3 (-90) 0 0, Scale $ V3 0.25 0.25 0.25
                          , Texture uranusTex, Shader program, Mesh sphereMesh
                          )


    (neptuneTex, _) <- loadTexture "resources/textures/2k_neptune.png"
    neptuneEty <- newEntity ( Planet
                          , Pos $ V3 44000 0 0, RotEuler $ V3 (-90) 0 0, Scale $ V3 0.25 0.25 0.25
                          , Texture neptuneTex, Shader program, Mesh sphereMesh
                          )

    cmapM_ \(Planet, Pos pos, ety) -> do
        selectEty <- newEntity ( Overlay
                            , Pos pos, RotEuler $ V3 0 0 0, Scale $ V3 1 1 1
                            , Texture selectTex, Shader fixedLighting, Mesh planeMesh
                            , Link ety
                            )
        return ()


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
        _ -> drawObjects (Filter @Mesh) >> drawTextBoxes Hud

drawObjects :: forall tag. (Get World IO tag, Members World IO tag) => tag -> WorldSystem ()
drawObjects _ = cmapM_ $ \(Camera, Pos camPos, CamRot pitch yaw) -> do
    cmapM_ $ \(_ :: tag, Texture tex, Shader sdr, Mesh mesh, Pos pos, mrot, mscale, mcolor) -> do
        let view = lookAt camPos (camPos + cameraForward pitch yaw) (V3 0 1 0)
        let proj = perspective (70 :: Float) (16.0/9) 0.1 1000000.0
        let (Scale size) = fromMaybe (Scale $ V3 1 1 1) mscale
        let scaleMat = scaled $ point size
        let rotMat = getRotMat $ fromMaybe (RotEuler $ V3 0 0 0) mrot
        let model = mkTransformationMat rotMat pos !*! scaleMat
        let (Color color) = fromMaybe (Color $ V4 1 1 1 1) mcolor


        withShader sdr do
            "model"         $== model
            "view"          $== view
            "projection"    $== proj

            "objectColor"   $== color
            "ambientColor"  $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)
            "viewPos"       $== camPos
            "lightPos"      $== (V3 0 0 0 :: V3 Float)
            "lightColor"    $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)

            liftIO $ GL.activeTexture $= GL.TextureUnit 0
            liftIO $ GL.textureBinding GL.Texture2D $= Just tex

            drawMeshAsset mesh -- size of the cube array

adjustOverlays = cmapM_ \(Camera, Pos camPos) -> do
    cmapM \(Overlay, Pos _, Link ety) -> do
        (Pos newPos) <- get ety
        let delta = camPos - newPos
        let dist = norm delta
        let dir = normalize delta
        let mat = matFromDir dir $ V3 0 1 0
        let s = dist * 0.05
        return (Overlay, Pos newPos, RotMat mat, Scale $ V3 s s s)


matFromDir :: V3 Float -> V3 Float -> M33 Float
matFromDir forward up = transpose mat
    where 
        side = cross forward up
        upn = cross forward side
        mat = V3 side upn forward

moveCamera :: (Float -> Float -> V3 Float) -> Float -> WorldSystem ()
moveCamera dir vec = cmap \(Camera, Pos pos, CamRot pitch yaw, GameSettings {cameraSpeed=(_, speed)}, Vel vel) ->
    Vel $ dir pitch yaw ^* vec ^* speed + vel

stopCam = return ()

inertia :: Double -> WorldSystem ()
inertia delta = cmapM \(Pos p, Vel v) -> do
    -- debugLog $ "updating velocity " <> show (v ^* realToFrac delta)
    return $ Pos $ p + (v ^* realToFrac delta)

handle :: Event -> WorldSystem ()
handle event = do
    textInput event
    Apecs.get global >>= \case 
        Playing -> playingInput event
        Terminal -> terminalInput event (Apecs.set global Playing)
        _ -> return ()


playingInput :: Event -> WorldSystem ()
playingInput (KeyEvent Key'Slash i ks mods) = Apecs.set global Terminal >> initTerminal
playingInput (KeyEvent Key'Escape i KeyState'Released mods) = liftIO exitSuccess
playingInput (KeyEvent Key'W i ks mods) = ifHeld ks (moveCamera cameraForward (1)) stopCam
playingInput (KeyEvent Key'A i ks mods) = ifHeld ks (moveCamera cameraRight (-1)) stopCam
playingInput (KeyEvent Key'S i ks mods) = ifHeld ks (moveCamera cameraForward (-1)) stopCam
playingInput (KeyEvent Key'D i ks mods) = ifHeld ks (moveCamera cameraRight (1)) stopCam
playingInput (KeyEvent Key'Q i ks mods) = ifHeld ks (moveCamera cameraUp (1)) stopCam
playingInput (KeyEvent Key'E i ks mods) = ifHeld ks (moveCamera cameraUp (-1)) stopCam
playingInput (KeyEvent Key'E i ks mods) = ifHeld ks (moveCamera cameraUp (-1)) stopCam
playingInput (CursorEvent x y) = do
    (CursorPos px py, GameSettings {cameraSpeed=(speed, _)}) <- get global

    let dx = realToFrac (x - px) * speed
    let dy = realToFrac (py - y) * speed
    cmap \(Camera, CamRot pitch yaw) -> (Camera, CamRot (clamp (-89) 89 (pitch + dy)) (yaw + dx), CursorPos x y)
playingInput _ = do
    return ()

runOrbit :: Double -> WorldSystem ()
runOrbit delta = cmap inner
    where 
        inner (Orbit rot rev rotS revS c r, Pos _) = ( Orbit rot' rev' rotS revS c r, Pos pos', RotQuat rotQ)
            where
                rot' = rot + rotS * realToFrac delta
                rev' = rev + revS * realToFrac delta
                x = r * sin rev'
                z = r * cos rev'
                pos' = V3 x 0 z
                rotQ = axisAngle (V3 0 1 0) rot'

runConsole :: Double -> WorldSystem ()
runConsole delta = return ()

step :: Double -> WorldSystem ()
step delta = do
    gameState <- get global
    adjustOverlays
    case gameState of
        Playing -> runOrbit delta >> inertia delta
        Terminal -> runConsole delta
        _ -> return ()



clamp :: (Ord a) => a -> a -> a -> a
clamp min max x | x < min = min
                | x > max = max
                | otherwise = x
