{-# LANGUAGE TypeFamilies          #-}
module Engine.Apecs.Types where

import Apecs
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Engine.Types
import qualified Graphics.Rendering.OpenGL as GL

newtype Pos = Pos (V3 Float) deriving Show
newtype Pos2D = Pos2D (V2 Float) deriving Show
newtype Rot = Rot (Either (V3 Float) (Quaternion Float)) deriving Show
newtype Scale = Scale (V3 Float) deriving Show
newtype Size2D = Size2D (V2 Float) deriving Show
data CamRot = CamRot Float Float deriving Show
data Hidden = Hidden deriving Show

newtype Texture = Texture GL.TextureObject deriving Show
newtype Shader = Shader GL.Program deriving Show
newtype Mesh = Mesh MeshAsset deriving Show
data Console = Console deriving Show

data Camera = Camera deriving Show
instance Component Camera where type Storage Camera = Unique Camera

data TerminalInput = TerminalInput deriving Show
instance Component TerminalInput where type Storage TerminalInput = Unique TerminalInput

newtype Command = Command (Maybe Entity) deriving Show -- thats the previous command


data TextBox = TextBox String Entity (V4 Float) Float deriving Show

data FormControl = FormControl deriving Show -- bool is focus
data Focus = Focus deriving Show 
newtype TextInput = TextInput Int deriving Show -- int is cursor pos