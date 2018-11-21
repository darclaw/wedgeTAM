{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib where

import Prelude as P
import Data.Maybe (maybeToList, catMaybes)
import Data.List (union)
import Data.List.Extra (nubOrd)
import Control.Monad (guard)

import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal
import Math.Geometry.GridMap as GMap
import Math.Geometry.GridMap.Lazy
import qualified Math.Geometry.Grid as Grid
--import Data.Map  

import Debug.Trace

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

-- board :: Math.Geometry.GridMap.Lazy.LGridMap Math.Geometry.Grid.SquareInternal.RectSquareGrid (FloorType (Tile Integer))

board' = lazyGridMap (rectSquareGrid 3 3) (repeat emptyTile)
board = insert (1,1) (Wedge North (Just (T 0 1 0 1)))  board'

testTiles = [T 0 1 0 1]
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
--Could have bug of Flat A B, Flat N N -> Flat N A instead of Flat N B
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

updateBoardAround j board t = do -- P.foldl (updateInDir') [board] [North, East, South, West]
   let bndr = []

   let nboards = updateInDir North j board t
   let bndr' = if not (null nboards) then (Grid.neighbour board j North):bndr else bndr
   let nboards' = nullHelp board nboards
   nboard <- nboards'

   let eboards = updateInDir East j nboard t
   let bndr'' = if not (null eboards) then (Grid.neighbour board j East):bndr' else bndr'
   let eboards' = nullHelp nboard eboards
   eboard <- eboards'

   let sboards = updateInDir South j eboard t
   let bndr''' = if not (null sboards) then (Grid.neighbour board j South):bndr'' else bndr''
   let sboards' = nullHelp eboard sboards
   sboard <- sboards'

   let wboards = updateInDir West j sboard t
   let bndr4 = if not (null wboards) then (Grid.neighbour board j West):bndr''' else bndr'''
   let wboards' = nullHelp sboard wboards
   return (wboards',bndr4)

nullHelp b []  = [b]
nullHelp b a  = a

--step board [] t = [([board],[])]   
--Need to change to folding around the boundry, not mapping over it.
step board bndry t = do
   i <- bndry 
   (boards,bndrs) <- updateBoardAround i  board t
   let bndrs' = catMaybes bndrs
   let nbndrs = union bndry bndrs'
   --let nbndrs = delete i nbndrs'

   return (boards, nbndrs)

simulate board bndry t 0 = [board]
simulate board [] t n = [board]
simulate board bndry t n = do
   let res = step board bndry t
   (nboards, nbndry) <- res
   nboard <- nboards
   simulate nboard nbndry t (n-1)


maybeHelp (a, Just b) = Just (a,b)
maybeHelp (a, Nothing) = Nothing
