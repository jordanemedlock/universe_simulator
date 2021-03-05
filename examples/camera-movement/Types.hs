{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Types where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Control.Lens.TH
import Linear
import Data.Time
import Data.IORef
import Engine


data EarthAssets = EarthAssets
    { _earthAssetsMesh :: MeshAsset
    , _earthAssetsTexture :: GL.TextureObject
    } deriving (Show)

makeFields ''EarthAssets

data Assets = Assets 
    { _assetsShader :: GL.Program
    , _assetsEarth :: EarthAssets
    } deriving (Show)

makeFields ''Assets


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

