{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Types where

import Apecs
import Linear
import Language.Haskell.TH
import Engine (Font(..))


newtype Mantissa = Mantissa Int deriving (Show, Eq, Num)
instance Semigroup Mantissa where (<>) = (+)
instance Monoid Mantissa where mempty = 0
instance Component Mantissa where type Storage Mantissa = Global Mantissa

newtype Translation = Translation (V3 Float) deriving (Show, Eq, Num)
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


newtype Vel = Vel (V3 Float) deriving Show

data Orbit = Orbit { rotation :: Float
                   , revolution :: Float
                   , rotationSpeed :: Float 
                   , revolutionSpeed :: Float 
                   , center :: V3 Float
                   , radius :: Float
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

newtype Color = Color (V4 Float) deriving Show

data GameSettings = GameSettings { cameraSpeed :: (Float, Float) -- (cursor, keyboard)
                                 , screenSize :: V2 Int
                                 , debug :: Bool
                                 } deriving Show
instance Semigroup GameSettings where (<>) = const
instance Monoid GameSettings where mempty = GameSettings (0.1, 5.0) (V2 1920 1080) True
instance Component GameSettings where type Storage GameSettings = Global GameSettings

data CursorPos = CursorPos Double Double deriving (Show, Eq)
instance Semigroup CursorPos where (CursorPos x1 y1) <> (CursorPos x2 y2) = CursorPos (x1+x2) (y1+y2)
instance Monoid CursorPos where mempty = CursorPos 0 0
instance Component CursorPos where type Storage CursorPos = Global CursorPos


data GameState = Paused | Playing | Closed | Terminal deriving (Show, Eq, Enum)
instance Semigroup GameState where (<>) = const
instance Monoid GameState where mempty = Playing
instance Component GameState where type Storage GameState = Global GameState

data Hud = Hud deriving Show

-- newtype Rand = Rand StdGen deriving (Show, Eq)
-- instance Semigroup Rand where (<>) = const
-- instance Monoid Rand where mempty = Rand (mkStdGen 0)
-- instance Component Rand where type Storage Rand = Global Rand

mapComponents :: [Name]
mapComponents = [ ''Hud, ''Orbit, ''Font, ''Vel, ''Planet, ''Overlay, ''Link, ''Color ]

nonMapComponents :: [Name]
nonMapComponents = [ ''GameSettings, ''CursorPos, ''GameState
                   , ''Earth, ''Moon, ''Sun
                   , ''Mantissa, ''Translation
                --    , ''Rand 
                   ]