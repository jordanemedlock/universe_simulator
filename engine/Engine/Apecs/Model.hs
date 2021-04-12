{-# LANGUAGE TupleSections,ConstraintKinds #-}
module Engine.Apecs.Model where

import qualified Data.Vector as V
import Engine.Apecs.Types 
import Engine.Types
import Apecs
import qualified Graphics.Rendering.OpenGL as GL
import Data.Vector ((!), (!?))

type GetSet w c = (Get w IO c, Set w IO c, Destroy w IO c) 
type HasGlTF w =    ( GetSet w Transform, GetSet w Mesh, GetSet w Texture
                    , GetSet w Rot, GetSet w Scale, GetSet w Pos, GetSet w Scene
                    , GetSet w HasParent, Get w IO EntityCounter
                    )

updateIndex :: [a] -> Int -> a -> [a]
updateIndex xs i x = take (i-1) xs <> [x] <> drop i xs

invertChildren :: [Maybe [Int]] -> [Maybe Int]
invertChildren childrens = value
    where
        helper :: (Int, Maybe [Int]) -> [Maybe Int] -> [Maybe Int]
        helper (i, Nothing) acc = acc
        helper (i, Just cs) acc = foldr (doubleHelper i) acc cs
        doubleHelper :: Int -> Int -> [Maybe Int] -> [Maybe Int]
        doubleHelper parent child acc = updateIndex acc child (Just parent)
        value = foldr helper (replicate (length childrens) Nothing) $ zip [0..] childrens

createGlTFEntities  :: HasGlTF w
                    => V.Vector 
                        ( Maybe [Int]
                        , Maybe Transform
                        , Maybe (MeshAsset, GL.TextureObject, GL.TextureObject)
                        , Maybe Rot
                        , Maybe Scale
                        , Maybe Pos
                        , Maybe Scene
                        )
                    -> SystemT w IO (V.Vector Entity)
createGlTFEntities ns = do
    let mchildren = V.toList $ V.map (\(mc, _,_,_,_,_,_) -> mc) ns
    let parents = V.fromList $ invertChildren mchildren
    nodes <- V.forM ns $ \(_, mtransform, mmeshAsset, mrot, mscale, mpos, mscene) -> do

        newEntity (mtransform, mtransform, (\(m, t, _) -> (Mesh m, Texture t)) <$> mmeshAsset, mrot, mscale, mpos, mscene)

    V.forM (V.zip nodes parents) $ \case
        (ety, Nothing) -> return ety
        (ety, Just parent) -> Apecs.modify ety ((,HasParent <$> nodes !? parent).(id :: Maybe Mesh -> Maybe Mesh)) >> return ety

