{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Types where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Control.Lens.TH
import Linear
import Data.Time
import Data.IORef


data EarthAssets = EarthAssets
    { _earthAssetsObject :: GL.VertexArrayObject
    , _earthAssetsNumTris :: GL.GLsizeiptr
    , _earthAssetsTexture :: GL.TextureObject
    }

makeFields ''EarthAssets

data Assets = Assets 
    { _assetsShader :: GL.Program
    , _assetsEarth :: EarthAssets
    }

makeFields ''Assets

data Rotation a = EulerRotation
    { _rotationVector :: V3 a -- Euler angles in degrees (for funsies)
    }
    | QuaternionRotation
    { _rotationQuaternion :: Quaternion a
    }

makeFields ''Rotation

data Transform a = Transform
    { _transformRotation :: Rotation a 
    , _transformPosition :: V3 a
    }

makeFields ''Transform

data Camera = Camera 
    { _cameraTransform :: Transform Float
    , _cameraFov :: Float 
    , _cameraRatio :: Float
    , _cameraNear :: Float
    , _cameraFar :: Float
    }

makeFields ''Camera

newtype Earth = Earth
    { _earthTransform :: Transform Float
    }

makeFields ''Earth

data GameState = GameState 
    { _gameStateCamera :: Camera
    , _gameStateEarth :: Earth
    , _gameStateLastTime :: UTCTime
    }

makeFields ''GameState

data Game = Game 
    { _gameWindow :: GLFW.Window 
    , _gameAssets :: Assets
    , _gameGameState :: IORef GameState
    }

makeFields ''Game

