module Day24 where

-- https://github.com/MatthiasCoppens/AOC2020/blob/master/day24/solution.hs

import Data.List ( group, sort )
import Data.Map ( Map )
import qualified Data.Map as M
    ( filter,
      fromList,
      keysSet,
      restrictKeys,
      unionsWith,
      withoutKeys )
import Data.Set as S
    ( delete, empty, insert, map, member, size, union, Set )

type Tile = (Int, Int)
type Count = Int
type BlackTiles = Set Tile -- contains the black tiles

parse :: String -> [Tile]
parse = fmap idTile .  lines

idTile :: String -> Tile
idTile "" = (0, 0)
idTile ('e' : cs) = (x + 1, y) where (x, y) = idTile cs
idTile ('w' : cs) = (x - 1, y) where (x, y) = idTile cs
idTile ('s' : 'e' : cs) = (x + 1, y - 1) where (x, y) = idTile cs
idTile ('s' : 'w' : cs) = (x, y - 1) where (x, y) = idTile cs
idTile ('n' : 'e' : cs) = (x, y + 1) where (x, y) = idTile cs
idTile ('n' : 'w' : cs) = (x - 1, y + 1) where (x, y) = idTile cs

countBlacks :: [Tile] -> Int
countBlacks = length . filter (odd . length) . group . sort

mkBlackTiles :: [Tile] -> BlackTiles
mkBlackTiles = foldr accumBlacks S.empty
  where
    accumBlacks x xs
      | x `S.member` xs = S.delete x xs
      | otherwise = S.insert x xs

-- The neighbouring coordinates of a tile turned into a cardinality map
neighbourhood :: Tile -> Map Tile Count
neighbourhood (x, y) =
  M.fromList
    [ ((x + dx, y + dy), 1) -- 1 occurrrence
      | (dx, dy) <- [(1, 0), (-1, 0), (1, -1), (0, -1), (0, 1), (-1, 1)]
    ]

step :: BlackTiles -> BlackTiles
step blackTiles = stayBlack `S.union`turnBlack
  where
    neighborCounts :: Map Tile Count
    neighborCounts = M.unionsWith (+) $ S.map neighbourhood blackTiles

    stayBlack :: BlackTiles
    -- M.keyset drops the counts, leaving only the tiles
    -- Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
    stayBlack = M.keysSet . M.filter (\n -> n == 1 || n == 2) $ neighborCounts `M.restrictKeys` blackTiles

    turnBlack :: BlackTiles
    -- Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
    turnBlack = M.keysSet . M.filter (== 2) $ neighborCounts `M.withoutKeys` blackTiles

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ countBlacks $ parse input
  print $ S.size $ mkBlackTiles $ parse input
  print $ S.size $ (!! 100) . iterate step . mkBlackTiles $ parse input

ex1 :: String
ex1 =
  "sesenwnenenewseeswwswswwnenewsewsw\n\
  \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
  \seswneswswsenwwnwse\n\
  \nwnwneseeswswnenewneswwnewseswneseene\n\
  \swweswneswnenwsewnwneneseenw\n\
  \eesenwseswswnenwswnwnwsewwnwsene\n\
  \sewnenenenesenwsewnenwwwse\n\
  \wenwwweseeeweswwwnwwe\n\
  \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
  \neeswseenwwswnwswswnw\n\
  \nenwswwsewswnenenewsenwsenwnesesenew\n\
  \enewnwewneswsewnwswenweswnenwsenwsw\n\
  \sweneswneswneneenwnewenewwneswswnese\n\
  \swwesenesewenwneswnwwneseswwne\n\
  \enesenwswwswneneswsenwnewswseenwsese\n\
  \wnwnesenesenenwwnenwsewesewsesesew\n\
  \nenewswnwewswnenesenwnesewesw\n\
  \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
  \neswnwewnwnwseenwseesewsenwsweewe\n\
  \wseweeenwnesenwwwswnew"
