{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Types where

import Apecs
import Linear
import Language.Haskell.TH
import Engine
import qualified Graphics.Rendering.OpenGL as GL


newtype Mantissa = Mantissa Int deriving (Show, Eq, Num)
instance Semigroup Mantissa where (<>) = (+)
instance Monoid Mantissa where mempty = 0
instance Component Mantissa where type Storage Mantissa = Global Mantissa

newtype Translation = Translation (V3 Double) deriving (Show, Eq, Num)
instance Semigroup Translation where (<>) = (+)
instance Monoid Translation where mempty = 0
instance Component Translation where type Storage Translation = Global Translation

meters = Mantissa 0
kiloms = Mantissa 3
megams = Mantissa 6
gigams = Mantissa 9
terams = Mantissa 12
petams = Mantissa 15
exams = Mantissa 18
zettams = Mantissa 21
yottams = Mantissa 24


newtype Vel = Vel (V3 Double) deriving Show
newtype RotVel = RotVel (V3 Double) deriving Show

data Orbit = Orbit { rotation :: Double
                   , revolution :: Double
                   , rotationSpeed :: Double 
                   , revolutionSpeed :: Double 
                   , center :: V3 Double
                   , radius :: Double
                   } deriving Show

data Sun = Sun deriving Show
instance Component Sun where type Storage Sun = Unique Sun

data Earth = Earth deriving Show
instance Component Earth where type Storage Earth = Unique Earth

data Moon = Moon deriving Show
instance Component Moon where type Storage Moon = Unique Moon

data Planet = Planet deriving Show

data Overlay = Overlay deriving Show

newtype Link = Link Entity deriving Show

newtype Color = Color (V4 Double) deriving Show

newtype Mass = Mass Double deriving Show -- havent picked units yet (prob kg)
data Kinematic = Kinematic deriving Show
data Static = Static deriving Show

data GameSettings = GameSettings { cameraSpeed :: (Double, Double) -- (cursor, keyboard)
                                 , screenSize :: V2 Int
                                 , gravConstant :: Double -- m^3 * kg^-1 * s^-2
                                 } deriving Show
instance Semigroup GameSettings where (<>) = const
instance Monoid GameSettings where mempty = GameSettings (0.1, 0.01) (V2 1920 1080) (6.6743*10^^(negate 11))
instance Component GameSettings where type Storage GameSettings = Global GameSettings

data DebugState = DebugState { dsDebug :: Bool
                             , dsPolygonMode :: GL.PolygonMode
                             , dsCameraMomentum :: Bool
                             , dsCursorEnabled :: Bool
                             } deriving Show
instance Semigroup DebugState where (<>) = const
instance Monoid DebugState where mempty = DebugState True GL.Fill True False
instance Component DebugState where type Storage DebugState = Global DebugState

data CursorPos = CursorPos Double Double deriving (Show, Eq)
instance Semigroup CursorPos where (CursorPos x1 y1) <> (CursorPos x2 y2) = CursorPos (x1+x2) (y1+y2)
instance Monoid CursorPos where mempty = CursorPos 0 0
instance Component CursorPos where type Storage CursorPos = Global CursorPos



data GameState = Paused | Playing | Closed | Terminal deriving (Show, Eq, Enum)
instance Semigroup GameState where (<>) = const
instance Monoid GameState where mempty = Playing
instance Component GameState where type Storage GameState = Global GameState


data Hud = Hud deriving Show

data Thrust = Thrust { thrustForward :: Double
                     , thrustRight :: Double
                     , thrustUp :: Double
                     , thrustRoll :: Double
                     , thrustPitch :: Double
                     , thrustYaw :: Double
                     } deriving Show



-- newtype Rand = Rand StdGen deriving (Show, Eq)
-- instance Semigroup Rand where (<>) = const
-- instance Monoid Rand where mempty = Rand (mkStdGen 0)
-- instance Component Rand where type Storage Rand = Global Rand

mapComponents :: [Name]
mapComponents = [ ''Hud, ''Orbit, ''Vel, ''Planet, ''RotVel
                , ''Overlay, ''Link, ''Color
                , ''Mass, ''Kinematic, ''Static, ''Thrust
                ]

nonMapComponents :: [Name]
nonMapComponents = [ ''GameSettings, ''CursorPos, ''GameState
                   , ''Earth, ''Moon, ''Sun
                   , ''Mantissa, ''Translation
                   , ''DebugState, ''Font
                --    , ''Rand 
                   ]