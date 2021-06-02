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
newtype Vel = Vel (V3 Double) deriving Show
newtype Size2D = Size2D (V2 Double) deriving Show
data CamRot = CamRot Double Double deriving Show
data Hidden = Hidden deriving Show
newtype Transform = Transform (M44 Double) deriving Show
newtype Scene = Scene String deriving Show
newtype ShaderName = ShaderName String deriving Show
newtype Texture = Texture TextureObject deriving Show
newtype FillColor = FillColor (V4 Float) deriving Show
data Object = Object deriving Show
data Hud = Hud deriving Show
data Menu = Menu deriving Show

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

newtype WindowTitle = WindowTitle String deriving Show
instance Semigroup WindowTitle where (<>) = const
instance Monoid WindowTitle where mempty = WindowTitle "Untitled window"
instance Component WindowTitle where type Storage WindowTitle = Global WindowTitle

-- TODO: this would be a good thing to automate with TH
newtype WindowSize = WindowSize (V2 Int) deriving Show
instance Semigroup WindowSize where (<>) = const
instance Monoid WindowSize where mempty = WindowSize (V2 1920 1080)
instance Component WindowSize where type Storage WindowSize = Global WindowSize


instance Semigroup GLFW.CursorInputMode where (<>) = const
instance Monoid GLFW.CursorInputMode where mempty = GLFW.CursorInputMode'Normal
instance Component GLFW.CursorInputMode where type Storage GLFW.CursorInputMode = Global GLFW.CursorInputMode


data GameState = Paused | Playing | Closed | Custom String deriving (Show, Eq)
instance Semigroup GameState where (<>) = const
instance Monoid GameState where mempty = Playing
instance Component GameState where type Storage GameState = Global GameState

instance Component GLFW.Window where type Storage GLFW.Window = Unique GLFW.Window

data LogLevel   = LogTrace 
                | LogDebug 
                | LogInfo 
                | LogWarn 
                | LogError 
                | LogFatal 
                deriving (Show, Eq, Ord, Enum)

data LogLoc = LogStdOut | LogStdErr | LogFile FilePath deriving (Show)

data Logger = Logger [LogLevel] LogLoc deriving (Show)


newtype CursorPos = CursorPos (V2 Double) deriving (Show, Eq)
instance Semigroup CursorPos where (CursorPos a) <> (CursorPos b) = CursorPos (a + b)
instance Monoid CursorPos where mempty = CursorPos 0
instance Component CursorPos where type Storage CursorPos = Global CursorPos
-- TODO: could probably use this later
-- data GlobalWithHist nat c = GlobalWithHist Int (IORef [c])
-- type instance Elem (GlobalWithHist nat c) = c
-- instance (KnownNat nat, Monoid c, MonadIO m) => ExplInit m (GlobalWithHist nat c) where
--     {-# INLINE explInit #-}
--     explInit = liftIO $ GlobalWithHist (fromIntegral (natVal $ proxy @nat)) <$> newIORef [mempty]
-- instance (MonadIO m) => ExplSet m (GlobalWithHist nat c) where
--     {-# INLINE explSet #-}
--     explSet (GlobalWithHist i ref) _ c = liftIO$ modifyIORef ref ((take i).(c:))
-- instance MonadIO m => ExplGet m (GlobalWithHist nat c) where
--     {-# INLINE explGet #-}
--     explGet (Global ref) _ = liftIO $ head <$> readIORef ref
--     {-# INLINE explExists #-}
--     explExists _ _ = return True

defaultMapComponents :: [Name]
defaultMapComponents = [ ''Pos, ''Pos2D, ''Rot, ''Scale, ''Size2D
                       , ''CamRot, ''Hidden, ''Texture, ''ShaderName
                       , ''Mesh, ''Console, ''Command, ''TextBox
                       , ''FormControl, ''Focus, ''TextInput
                       , ''Transform, ''Scene, ''HasParent, ''FontName
                       , ''FillColor, ''Object, ''Hud, ''Menu, ''Logger
                       , ''Vel
                       ]

defaultAllComponents :: [Name]
defaultAllComponents =  [ ''Camera, ''TerminalInput, ''WindowTitle
                        , mkName "CursorInputMode", ''GameState 
                        , mkName "Window", ''WindowSize, ''Font
                        , mkName "Program", ''CursorPos
                        ] <> defaultMapComponents