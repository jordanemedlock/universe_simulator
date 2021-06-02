{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Main where

import Engine.Prelude

makeMapComponents $ defaultMapComponents
makeWorld "World" $ defaultAllComponents


main = do
    world <- initWorld

    runWith world $ do
        set global  ( WindowTitle "Minecraft clone"
                    )
        newEntity (Logger [LogTrace, LogDebug, LogWarn, LogInfo] LogStdOut)
        newEntity (Logger [LogError, LogFatal] LogStdErr)

        

        play initialize draw handle step
    

initialize = do
    disableCursor

    initCharacter
    initChunks
    initHud
    
initCharacter = do
    let camera = (Camera, Pos $ V3 10 0 1, CamRot 0 0, Vel $ V3 0 0 0)
    characterEty <- newEntity (camera)

    return ()

initChunks = do
    cube <- createCubeAsset 
    firstCube <- newEntity (Mesh cube, FillColor green, Pos 0, RotEuler 0, Scale 1, ShaderName "lighting")

    return ()

initHud = do
    return ()

draw = do
    drawObjects (Filter @Mesh)
    drawTextBoxes (Filter @Hud)
    drawSprites (Filter @Hud)

    ifPaused do
        drawTextBoxes (Filter @Menu)
        drawSprites (Filter @Menu)

handle event = do
    textInput event 
    handleGameState event $ \case 
        (Playing, Paused) -> enableCursor
        (Paused, Playing) -> disableCursor
        _ -> return ()

    ifPlaying do
        cameraInput event
        handleCharacterInput event
    
    ifPaused do
        handleMenuInput event


cameraInput (CursorEvent x y) = do
    (CursorPos (V2 px py)) <- get global

    let dx = realToFrac (x - px) * 0.1
    let dy = realToFrac (py - y) * 0.1
    cmap \(Camera, CamRot pitch yaw) -> (Camera, CamRot (clamp (-89) 89 (pitch + dy)) (yaw + dx), CursorPos (V2 x y))

cameraInput _ = return ()

step delta = do
    ifPlaying do
        updateCharacter delta
        updateWorld delta
        updateHud delta
    
    ifPaused do
        updateMenu delta


handleCharacterInput event = do
    return ()

handleMenuInput event = do
    return ()

updateCharacter delta = do
    return ()

updateWorld delta = do
    return ()

updateHud delta = do
    return ()

updateMenu delta = do
    return ()