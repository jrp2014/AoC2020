module Main where

import Data.Bits ( Bits((.&.)) )
import Data.Bool ( bool )
import qualified Data.IntMap as M ( (!), fromListWith, IntMap )
import Data.List.Extra
    ( foldl', sort, sortOn, transpose, foldl1', groupOn, splitOn )
import Data.Tuple.Extra ( dupe )

-- https://github.com/haskelling/aoc2020/blob/main/20b.hs

type Tile = [[Bool]]

type TileNum = Int

count ::
  Eq a =>
  -- | The value to look for
  a ->
  -- | The list to look in
  [a] ->
  -- | The number of times the value is found in the list
  Int
count c = length . filter (== c)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ = id
applyN n f = foldr1 (.) $ replicate n f

main :: IO ()
main = do
  input <- Data.List.Extra.splitOn [""] . lines <$> readFile "input.txt"
  print $ part2 input

data Orientation = Ori Bool Int
  deriving (Show, Eq, Read, Ord)

rotgrid :: Tile -> Tile
rotgrid = transpose . reverse

rotgridn :: Int -> Tile -> Tile
rotgridn n = applyN n rotgrid

orients :: [Orientation]
orients = [Ori flipped nrots | flipped <- [False, True], nrots <- [0 .. 3]]

orient :: Orientation -> Tile -> Tile
orient (Ori False n) = rotgridn n
orient (Ori True n) = rotgridn n . reverse

getorients :: Tile -> [(Orientation, Tile)]
getorients g = [(o, orient o g) | o <- orients]

-- Encode a row ro column as an Int
boolsToInt :: [Bool] -> Int
boolsToInt = foldl' (\x y -> x * 2 + bool 0 1 y) 0

monster :: Tile
monster =
  map
    (map (== '#'))
    ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]

monsterSig :: Int
monsterSig = mkSig monster

mkSig :: Tile -> Int
mkSig ss = boolsToInt $ concatMap (take 20) $ take 3 ss

findMonsters :: Tile -> Int
findMonsters ss =
  maximum $ map (uncurry findMonsters' . dupe . snd) $ getorients ss
  where
    findMonsters' ss0 ss =
      if length ss < 3
        then 0
        else
          (if mkSig ss .&. monsterSig == monsterSig then 1 else 0)
            + if length (head ss) > 20
              then findMonsters' ss0 (map tail ss)
              else let ss0' = tail ss0 in findMonsters' ss0' ss0'

parseTiles :: [String] -> (TileNum, Tile)
parseTiles (t : s) = (read $ init t', s')
  where
    (_ : t' : _) = words t

    s' = map (map (== '#')) s

t2of3 :: (a, b, c) -> b
t2of3 (_, x, _) = x

showMap :: Tile -> String
showMap = unlines . map (map (bool '.' '#'))

part2 :: [[String]] -> Int
part2 s =
  count True (concat completeGrid) - findMonsters completeGrid
    * count
      True
      (concat monster)
  where
    tiles :: [(TileNum, Tile)]
    tiles = parseTiles <$> filter (not . null) s

    tileIntMapping :: [(Int, (Orientation, TileNum, Tile))]
    tileIntMapping = concatMap getInts tiles

    getInts :: (TileNum, Tile) -> [(Int, (Orientation, TileNum, Tile))]
    getInts (tnum, tile) =
      map (\(o, tile') -> (boolsToInt $ head tile', (o, tnum, tile'))) $
        getorients tile

    uniqueEdges :: [[(Int, (Orientation, TileNum, Tile))]]
    uniqueEdges = filter ((== 1) . length) $ groupOn fst $ sort tileIntMapping

    -- corner tiles are tiles with 4 unique edges (2 edges * 2 orientations)
    cornerTiles :: [[(Orientation, TileNum, Tile)]]
    cornerTiles =
      filter ((== 4) . length) $
        groupOn t2of3 $
          sortOn t2of3 $
            map
              (snd . head)
              uniqueEdges

    tileMap :: M.IntMap [(TileNum, Tile)]
    tileMap =
      M.fromListWith (++) $
        map (\(edge, (_, tnum, tile)) -> (edge, [(tnum, tile)])) tileIntMapping

    stTiles :: [(Orientation, TileNum, Tile)]
    stTiles = filter (\(Ori flipped _, _, _) -> not flipped) $ head cornerTiles

    stGrid :: Tile
    stGrid =
      let [(o1, t1), (o2, t2)] = map (\(Ori _ n, _, t) -> (n, t)) stTiles
       in if o2 == succ o1 `mod` 4 then t1 else t2

    stTile :: (TileNum, Tile)
    stTile = (t2of3 $ head stTiles, stGrid)

    belowTiles :: (TileNum, Tile) -> [(TileNum, Tile)]
    belowTiles (n, t) =
      case filter ((/= n) . fst) $ tileMap M.! boolsToInt (last t) of
        [nexttile] -> (n, t) : belowTiles nexttile
        _ -> [(n, t)]

    rightTiles :: (TileNum, Tile) -> [Tile]
    rightTiles (n, t) = map (transpose . snd) $ belowTiles (n, transpose t)

    mid :: [a] -> [a]
    mid = tail . init

    allTiles :: [[Tile]]
    allTiles = map (map (mid . map mid) . rightTiles) $ belowTiles stTile

    allTilesRows :: [Tile]
    allTilesRows = map (foldl1' (zipWith (++))) allTiles

    completeGrid :: Tile
    completeGrid = concat allTilesRows
