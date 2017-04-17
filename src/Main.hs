{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe

data Color = Blue | Green | Yellow | Grey deriving (Eq)
data Category = Restaurant | Business | School | Skyscraper | Dealership | Airport deriving (Eq)

data Coord = Coord Int Int deriving (Show, Eq)

instance Num Coord where
  Coord q1 r1 + Coord q2 r2 = Coord (q1 + q2) (r1 + r2)
  Coord q1 r1 * Coord q2 r2 = Coord (q1 * q2) (r1 * r2)
  abs _ = undefined
  signum _ = undefined
  fromInteger i = Coord (fromInteger i) (fromInteger i)
  negate (Coord q r) = Coord (-q) (-r)

data Tile = Tile
  { _name :: String,
    _color :: Color,
    _category :: Maybe Category,
    _price :: Int,
    _effect :: Metro -> Coord -> Metro }

instance Eq Tile where
  t1 == t2 = (_name t1) == (_name t2)

instance Show Tile where
  show tile = show (_name tile)

data Metro = Metro
  { _money :: Int,
    _population :: Int,
    _income :: Int,
    _reputation :: Int,
    _tiles :: [(Coord, Tile)] } deriving (Show)

makeLenses ''Tile
makeLenses ''Metro

startingMetro = Metro
  { _money = 15,
    _population = 2,
    _income = 0,
    _reputation = 1,
    _tiles = [(Coord 0 0, allTiles !! 0), (Coord 0 1, allTiles !! 2), (Coord 0 2, allTiles !! 1)] }

adjacents = [Coord 1 0, Coord 1 (-1), Coord 0 (-1), Coord (-1) 0, Coord (-1) 1, Coord 0 1]

neighbors :: Coord -> [Coord]
neighbors coord = map (\x -> coord + x) adjacents

adjacentTiles :: Metro -> Coord -> [Tile]
adjacentTiles metro c =  unwrapJusts . map get $ neighbors c
  where
    unwrapJusts = map fromJust . filter (/= Nothing)
    get = flip lookup $ _tiles metro

countTilesOfColor :: [Tile] -> Color -> Int
countTilesOfColor tiles c = length $ filter (\x -> _color x == c) tiles

countAdjacentsOfColor :: Metro -> Coord -> Color -> Int
countAdjacentsOfColor metro coord color = countTilesOfColor (adjacentTiles metro coord) color

increase l n = over l (+ n)
decrease l n = over l (\x -> x - n)

heavyFactoryEffect m c = decrease reputation (greens + greys) . increase income 1 $ m
  where
    greens = countAdjacentsOfColor m c Green
    greys = countAdjacentsOfColor m c Grey

communityParkEffect m c = increase reputation (yellows + greens + blues) . decrease income 1 $ m
  where
    greens = countAdjacentsOfColor m c Green
    yellows = countAdjacentsOfColor m c Yellow
    blues = countAdjacentsOfColor m c Blue


allTiles = [Tile { _name = "Suburbs",
                _color = Green,
                _category = Nothing,
                _price = 3,
                _effect = \m _ -> increase population 2 m},
         Tile { _name = "Heavy Factory",
                _color = Yellow,
                _category = Nothing,
                _price = 3,
                _effect = heavyFactoryEffect},
         Tile { _name = "Community Park",
                _color = Grey,
                _category = Nothing,
                _price = 4,
                _effect = \m _ -> communityParkEffect}
        ]

main :: IO ()
main = putStrLn "hello world"
