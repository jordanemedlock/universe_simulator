{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
module Engine.Apecs.Types where

import Apecs
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Engine.Types
import qualified Graphics.Rendering.OpenGL as GL
import Language.Haskell.TH.Syntax
import Engine.Vector (rotationMatrix)

newtype Pos = Pos (V3 Double) deriving Show
newtype Pos2D = Pos2D (V2 Double) deriving Show
data Rot = RotEuler (V3 Double) 
         | RotQuat (Quaternion Double)
         | RotMat (M33 Double) 
         deriving Show

getRotMat (RotEuler e) = rotationMatrix e
getRotMat (RotQuat q) = fromQuaternion q
getRotMat (RotMat m) = m

newtype Scale = Scale (V3 Double) deriving Show
newtype Size2D = Size2D (V2 Double) deriving Show
data CamRot = CamRot Double Double deriving Show
data Hidden = Hidden deriving Show
newtype Transform = Transform (M44 Double) deriving Show
newtype Scene = Scene String deriving Show

newtype Texture = Texture GL.TextureObject deriving Show
newtype Shader = Shader GL.Program deriving Show
newtype Mesh = Mesh MeshAsset deriving Show
data Console = Console deriving Show

data Camera = Camera deriving Show
instance Component Camera where type Storage Camera = Unique Camera

data TerminalInput = TerminalInput deriving Show
instance Component TerminalInput where type Storage TerminalInput = Unique TerminalInput

newtype Command = Command (Maybe Entity) deriving Show -- thats the previous command


data TextBox = TextBox String (V4 Double) Double deriving Show

data FormControl = FormControl deriving Show -- bool is focus
data Focus = Focus deriving Show 
newtype TextInput = TextInput Int deriving Show -- int is cursor pos
newtype HasParent = HasParent Entity deriving Show
newtype FontName = FontName String deriving Show

defaultMapComponents :: [Name]
defaultMapComponents = [ ''Pos, ''Pos2D, ''Rot, ''Scale, ''Size2D
                       , ''CamRot, ''Hidden, ''Texture, ''Shader
                       , ''Mesh, ''Console, ''Command, ''TextBox
                       , ''FormControl, ''Focus, ''TextInput
                       , ''Transform, ''Scene, ''HasParent, ''FontName
                       ]

defaultAllComponents :: [Name]
defaultAllComponents = [ ''Camera, ''TerminalInput ] <> defaultMapComponents