{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib where


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

board = lazyGridMap (rectSquareGrid 1 2) [emptyTile, emptyTile]
--neighbors b i = neighbors

neighVals b i = Prelude.map (b !) (Grid.neighbours b i)
