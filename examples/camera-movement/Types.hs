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
    } deriving (Show)

makeFields ''EarthAssets

data Assets = Assets 
    { _assetsShader :: GL.Program
    , _assetsEarth :: EarthAssets
    } deriving (Show)

makeFields ''Assets

data Rotation a = EulerRotation
    { _rotationEuler :: V3 a -- Euler angles in degrees (for funsies)
    }
    | QuaternionRotation
    { _rotationQuaternion :: Quaternion a
    } deriving (Show)

makeFields ''Rotation

data Transform a = Transform
    { _transformRotation :: Rotation a 
    , _transformPosition :: V3 a
    } deriving (Show)

makeFields ''Transform

data Camera = Camera 
    { _cameraPitch :: Float
    , _cameraYaw :: Float
    , _cameraPosition :: V3 Float 
    , _cameraFov :: Float 
    , _cameraRatio :: Float
    , _cameraNear :: Float
    , _cameraFar :: Float
    } deriving (Show)

makeFields ''Camera

newtype Earth = Earth
    { _earthTransform :: Transform Float
    } deriving (Show)

makeFields ''Earth

data GameMode = Playing | Paused | Stopped deriving Show

data GameState = GameState 
    { _gameStateCamera :: Camera
    , _gameStateEarth :: Earth
    , _gameStateLastTime :: UTCTime
    , _gameStateLastCursorPos :: Maybe (V2 Double)
    , _gameStateMode :: GameMode
    } deriving (Show)

makeFields ''GameState


data Game = Game 
    { _gameWindow :: GLFW.Window 
    , _gameAssets :: Assets
    , _gameGameState :: IORef GameState
    }

makeFields ''Game

