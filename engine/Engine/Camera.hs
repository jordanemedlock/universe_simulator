{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Engine.Camera where

import Linear
import Control.Lens
import Control.Lens.TH
import Engine.Types

degToRad :: Float -> Float 
degToRad = (*pi).(/180)

cameraForward :: Camera -> V3 Float
cameraForward cam = normalize $ V3 dx dy dz
    where 
        dx = cos (degToRad $ cam^.yaw) * cos (degToRad $ cam^.pitch)
        dy = sin (degToRad $ cam^.pitch)
        dz = sin (degToRad $ cam^.yaw) * cos (degToRad $ cam^.pitch)

cameraRight :: Camera -> V3 Float
cameraRight cam = normalize $ cross (cameraForward cam) (V3 0 1 0)

cameraUp :: Camera -> V3 Float
cameraUp cam = normalize $ cross (cameraRight cam) (cameraForward cam)

moveCameraForward :: Float -> Camera -> Camera
moveCameraForward speed cam = cam & position +~ cameraForward cam ^* speed

moveCameraRight :: Float -> Camera -> Camera
moveCameraRight speed cam = cam & position +~ cameraRight cam ^* speed

moveCameraUp :: Float -> Camera -> Camera
moveCameraUp speed cam = cam & position +~ cameraUp cam ^* speed


projection :: Camera -> M44 Float
projection cam = perspective (cam^.fov) (cam^.ratio) (cam^.near) (cam^.far)

cameraMatrix :: Camera -> M44 Float
cameraMatrix cam = lookAt (cam^.position) (cam^.position + cameraForward cam) (V3 0 1 0)
