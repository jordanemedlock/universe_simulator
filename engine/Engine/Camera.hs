{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Engine.Camera where

import Linear
import Control.Lens
import Control.Lens.TH
import Engine.Types

degToRad :: Float -> Float 
degToRad = (*pi).(/180)

cameraForward :: Float -> Float -> V3 Float
cameraForward pitch yaw = normalize $ V3 dx dy dz
    where 
        dx = cos (degToRad yaw) * cos (degToRad pitch)
        dy = sin (degToRad pitch)
        dz = sin (degToRad yaw) * cos (degToRad pitch)

cameraRight :: Float -> Float -> V3 Float
cameraRight pitch yaw = normalize $ cross (cameraForward pitch yaw) (V3 0 1 0)

cameraUp :: Float -> Float -> V3 Float
cameraUp pitch yaw = normalize $ cross (cameraRight pitch yaw) (cameraForward pitch yaw)

-- moveCameraForward :: Float -> Camera -> Camera
-- moveCameraForward speed cam = cam & position +~ (cameraForward (cam^.pitch) (cam^.yaw)) ^* speed

-- moveCameraRight :: Float -> Camera -> Camera
-- moveCameraRight speed cam = cam & position +~ cameraRight (cam^.pitch) (cam^.yaw) ^* speed

-- moveCameraUp :: Float -> Camera -> Camera
-- moveCameraUp speed cam = cam & position +~ cameraUp (cam^.pitch) (cam^.yaw) ^* speed


-- projection :: Camera -> M44 Float
-- projection cam = perspective (cam^.fov) (cam^.ratio) (cam^.near) (cam^.far)

-- cameraMatrix :: Camera -> M44 Float
-- cameraMatrix cam = lookAt (cam^.position) (cam^.position + (cameraForward (cam^.pitch) (cam^.yaw))) (V3 0 1 0)
