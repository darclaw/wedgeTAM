{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib where

import Prelude as P
import Data.Maybe (maybeToList)
import Control.Monad (guard)

import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal
import Math.Geometry.GridMap as GMap
import Math.Geometry.GridMap.Lazy
import qualified Math.Geometry.Grid as Grid
--import Data.Map  

someFunc :: IO ()
someFunc = putStrLn "someFunc"

f .> g = g f

-- assumes that the board is tiled by squares
data FloorType t = Flat (Maybe t) (Maybe t)
                   -- | Wedge (Grid.Direction g) (Maybe t)
                   | Wedge (SquareDirection) (Maybe t)
                   deriving (Show)
--deriving instance Show (FloorType SquareDirection Int)

getTiles (Flat (Just x) (Just y)) = [x,y]
getTiles (Flat Nothing (Just x)) = [x]
getTiles (Wedge _ (Just x)) = [x]
getTiles _ = []

emptyTile = Flat Nothing Nothing
-- isEmptyFrom dir Source Target
--isEmptyFrom d (North 

--data Board g t = GridMap g (FloorType t)

board' = lazyGridMap (rectSquareGrid 3 3) (repeat emptyTile)
board = insert (1,1) (Wedge North (Just (T 0 1 0 1)))  board'
--neighbors b i = neighbors

neighVals b i = P.map (b !) (Grid.neighbours b i)

neighAlls b i = P.map (allInfoFrom b i) (Grid.neighbours b i)

allInfoFrom b i j = (b ! j, j, head (Grid.directionTo b i j))

directedNeighAll b i d = (allInfoFrom b i) <$> Grid.neighbour b i d

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
compatible North (Wedge South _) (Wedge North Nothing) = True
compatible North (Wedge South _) (Flat Nothing  Nothing) = True
compatible North (Flat _ (Just _)) (Wedge South Nothing) = True

compatible East (Flat _ _) (Wedge East Nothing) = True
compatible East (Wedge East _) (Flat Nothing (Just _)) = True
compatible East (Wedge East _) (Wedge West Nothing) = True
compatible East (Wedge West _) (Wedge East Nothing) = True
compatible East (Wedge West _) (Flat Nothing  Nothing) = True
compatible East (Flat _ (Just _)) (Wedge West Nothing) = True

compatible South (Flat _ _) (Wedge South Nothing) = True
compatible South (Wedge South _) (Flat Nothing (Just _)) = True
compatible South (Wedge South _) (Wedge North Nothing) = True
compatible South (Wedge North _) (Wedge South Nothing) = True
compatible South (Wedge North _) (Flat Nothing  Nothing) = True
compatible South (Flat _ (Just _)) (Wedge North Nothing) = True

compatible West (Flat _ _) (Wedge West Nothing) = True
compatible West (Wedge West _) (Flat Nothing (Just _)) = True
compatible West (Wedge West _) (Wedge East Nothing) = True
compatible West (Wedge East _) (Wedge West Nothing) = True
compatible West (Wedge East _) (Flat Nothing  Nothing) = True
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

--updateInDir :: SquareDirection -> (Int,Int) -> LGridMap RectSquareGrid (FloorType (Tile t)) -> [Tile t] -> [LGridMap RectSquareGrid (FloorType (Tile t))]
updateInDir d i b t = do
   let source = b ! i
   sourceTile <- getTiles source
   (target,j,_) <- maybeToList $ directedNeighAll b i d
   guard (compatible d source target)
   ntile <- caniditeTiles d sourceTile t
   let nboard = updateBoard j ntile b
   return nboard

updateBoard j ntile b = adjust placeTile j b
   where placeTile (Wedge x Nothing) = Wedge x (Just ntile)
         placeTile (Flat Nothing Nothing) = Flat Nothing (Just ntile)
         placeTile (Flat Nothing x) = Flat (Just ntile) x
         placeTile x = x
