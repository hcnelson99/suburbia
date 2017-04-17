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

data MetroStats = MetroStats
  { _money :: Int,
    _population :: Int,
    _income :: Int,
    _reputation :: Int } deriving (Show)

instance Num MetroStats where
  MetroStats {_money=m1, _population=p1, _income=i1, _reputation=r1} + MetroStats {_money=m2, _population=p2, _income=i2, _reputation=r2} = MetroStats {_money= m1 + m2, _population=p1 + p2, _income=i1 + i2, _reputation=r1 + r2}
  MetroStats {_money=m1, _population=p1, _income=i1, _reputation=r1} * MetroStats {_money=m2, _population=p2, _income=i2, _reputation=r2} = MetroStats {_money= m1 * m2, _population=p1 * p1, _income=i1 * i2, _reputation=r1 * r2}
  abs _ = undefined
  signum _ = undefined
  fromInteger i = MetroStats {_money= fromInteger i, _population=fromInteger i, _income=fromInteger i, _reputation=fromInteger i}
  negate MetroStats {_money=m, _population=p, _income=i, _reputation=r} = MetroStats {_money= -m, _population= -p, _income= -i, _reputation= -r}

makeLenses ''MetroStats

data Tile = Tile
  { _name :: String,
    _color :: Color,
    _category :: Maybe Category,
    _price :: Int,
    _effect :: Metro -> Coord -> MetroStats }

instance Eq Tile where
  t1 == t2 = _name t1 == _name t2

instance Show Tile where
  show tile = "{Tile: " ++ show (_name tile) ++ "}"



data Metro = Metro
    { _tiles :: [(Coord, Tile)],
      _ledger :: [MetroStats] } deriving (Show)

makeLenses ''Tile
makeLenses ''Metro

startingMetro = Metro
  { _tiles = [(Coord 0 0, tile "Suburbs"), (Coord 0 1, tile "Community Park"), (Coord 0 2, tile "Heavy Factory")],
    _ledger = [increase money 15]}

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

increase :: ASetter MetroStats MetroStats Int Int -> Int -> MetroStats
increase l n = over l (+ n) 0
decrease :: ASetter MetroStats MetroStats Int Int -> Int -> MetroStats
decrease l n = over l (\x -> x - n) 0

heavyFactoryEffect m c = decrease reputation (greens + greys) + increase income 1
  where
    greens = countAdjacentsOfColor m c Green
    greys = countAdjacentsOfColor m c Grey

communityParkEffect m c = increase reputation (yellows + greens + blues) + decrease income 1
  where
    greens = countAdjacentsOfColor m c Green
    yellows = countAdjacentsOfColor m c Yellow
    blues = countAdjacentsOfColor m c Blue


allTiles = [Tile { _name = "Suburbs",
                _color = Green,
                _category = Nothing,
                _price = 3,
                _effect = \_ _ -> increase population 2},
         Tile { _name = "Heavy Factory",
                _color = Yellow,
                _category = Nothing,
                _price = 3,
                _effect = heavyFactoryEffect},
         Tile { _name = "Community Park",
                _color = Grey,
                _category = Nothing,
                _price = 4,
                _effect = communityParkEffect}
        ]

tile :: String -> Tile
tile name = fromJust $ tile' name allTiles
  where
    tile' _ [] = Nothing
    tile' n (x:xs) = if n == _name x then Just x else tile' n xs

score :: Metro -> MetroStats
score metro = (foldl1 (+) (_ledger metro)) + (foldl1 (+) $ map (\(c, t) -> (_effect t) metro c) (_tiles metro))

main :: IO ()
main = putStrLn "hello world"
