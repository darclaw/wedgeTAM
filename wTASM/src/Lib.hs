{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib where

import Prelude as P
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal
import Math.Geometry.GridMap as GMap
import Math.Geometry.GridMap.Lazy
import qualified Math.Geometry.Grid as Grid
--import Data.Map  

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- assumes that the board is tiled by squares
data FloorType t = Flat (Maybe t) (Maybe t)
                   -- | Wedge (Grid.Direction g) (Maybe t)
                   | Wedge (SquareDirection) (Maybe t)
                   deriving (Show)
--deriving instance Show (FloorType SquareDirection Int)

emptyTile = Flat Nothing Nothing
-- isEmptyFrom dir Source Target
--isEmptyFrom d (North 

--data Board g t = GridMap g (FloorType t)

board = lazyGridMap (rectSquareGrid 30 30) (repeat emptyTile)
--neighbors b i = neighbors

neighVals b i = P.map (b !) (Grid.neighbours b i)

neighAlls b i = P.map (\j -> (b ! j, j, head (Grid.directionTo b i j))) (Grid.neighbours b i)

--isOpen s t 
-- compatible (Wedge North _) (Wedge South Nothing) = True 
-- compatible (Wedge West _) (Wedge East Nothing) = True 
-- compatible (Wedge South _) (Wedge North Nothing) = True 
-- compatible (Wedge East _) (Wedge West Nothing) = True 
-- compatible (Wedge _ _) (Flat Nothing (Just _)) = True
-- compatible (Flat _ _) (Flat Nothing Nothing) = True
-- compatible (Flat (Just _) _) (Flat Nothing (Just _)) = True
-- compatible dir source target
compatible _ (Flat _ _) (Flat _ Nothing) = True
compatible _ (Flat _ (Just _)) (Flat Nothing (Just _)) = True
compatible North (Flat _ _) (Wedge North Nothing) = True
compatible North (Wedge North _) (Flat Nothing (Just _)) = True
compatible North (Wedge North _) (Wedge South Nothing) = True
compatible North (Flat _ (Just _)) (Wedge South Nothing) = True

compatible East (Flat _ _) (Wedge East Nothing) = True
compatible East (Wedge East _) (Flat Nothing (Just _)) = True
compatible East (Wedge East _) (Wedge West Nothing) = True
compatible East (Flat _ (Just _)) (Wedge West Nothing) = True

compatible South (Flat _ _) (Wedge South Nothing) = True
compatible South (Wedge South _) (Flat Nothing (Just _)) = True
compatible South (Wedge South _) (Wedge North Nothing) = True
compatible South (Flat _ (Just _)) (Wedge North Nothing) = True

compatible West (Flat _ _) (Wedge West Nothing) = True
compatible West (Wedge West _) (Flat Nothing (Just _)) = True
compatible West (Wedge West _) (Wedge East Nothing) = True
compatible West (Flat _ (Just _)) (Wedge East Nothing) = True

compatible _ _ _ = False


data Tile t = T t t t t
            deriving (Show)

--sticks dir source targe
sticks North (T x _ _ _) (T _ _ y _) = x==y
sticks East (T _ x _ _) (T _ _ _ y) = x==y
sticks South (T _ _ x _) (T y _ _ _) = x==y
sticks West (T _ _ _ x) (T _ y _ _) = x==y

caniditeTiles dir sourceTile tileSet = P.filter (sticks dir sourceTile) tileSet

tilesDeterministic tileSet = all (\(d,s) -> isOneOrNone (caniditeTiles d s tileSet)) [(d,s) | d<- [North,East,West,South], s<- tileSet]
   where isOneOrNone [] = True
         isOneOrNone [_] = True
         isOneOrNone _ = False

--step tileset board bndry = do
--   (cord,tile) <- bndry

