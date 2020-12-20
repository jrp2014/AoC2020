module Day20 where

import Data.List ( group, sort, transpose )
import Data.List.Split

data Tile = Tile { tileID :: Int, pixels :: [ [ Char ] ] }
    deriving ( Show )

parseTile :: String -> Tile
parseTile s = Tile (read . init . last $ words idLine) tile
  where
    (idLine : tile) = lines s -- NB may neeed to remove trailing empty line

parseTiles :: String -> [ Tile ]
parseTiles = map parseTile . splitOn "\n\n"

-- including flipped borders
borders :: [ [ a ] ] -> [ [ a ] ]
borders xy = edges ++ map reverse edges
  where
    edges = [ head xy, last xy, head xy', last xy' ]

    xy' = transpose xy

countOccurrences :: Ord a => [ a ] -> [ ( a, Int ) ]
countOccurrences = map (\x -> ( head x, length x )) . group . sort

keepOccurrences :: Ord b => Int -> [ b ] -> [ b ]
keepOccurrences n = map fst . filter (\( _, c ) -> c == n) . countOccurrences

part1 :: String -> Int
part1 s = product . keepOccurrences 4 $ concatMap findTileIds uniqueBorderOccurrences
  where
    tiles :: [Tile]
    tiles = parseTiles s

    borderOccurrences :: [[Char]]
    borderOccurrences = concatMap (borders . pixels) tiles

    uniqueBorderOccurrences :: [[Char]]
    uniqueBorderOccurrences = keepOccurrences 1 borderOccurrences

    findTileIds :: [Char] -> [Int]
    findTileIds border = tileID <$> filter (\tile -> border `elem` borders (pixels tile))
        tiles

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ part1 input

ex1 :: String
ex1 = "Tile 2311:\n\
  \..##.#..#.\n\
  \##..#.....\n\
  \#...##..#.\n\
  \####.#...#\n\
  \##.##.###.\n\
  \##...#.###\n\
  \.#.#.#..##\n\
  \..#....#..\n\
  \###...#.#.\n\
  \..###..###\n\
  \\n\
  \Tile 1951:\n\
  \#.##...##.\n\
  \#.####...#\n\
  \.....#..##\n\
  \#...######\n\
  \.##.#....#\n\
  \.###.#####\n\
  \###.##.##.\n\
  \.###....#.\n\
  \..#.#..#.#\n\
  \#...##.#..\n\
  \\n\
  \Tile 1171:\n\
  \####...##.\n\
  \#..##.#..#\n\
  \##.#..#.#.\n\
  \.###.####.\n\
  \..###.####\n\
  \.##....##.\n\
  \.#...####.\n\
  \#.##.####.\n\
  \####..#...\n\
  \.....##...\n\
  \\n\
  \Tile 1427:\n\
  \###.##.#..\n\
  \.#..#.##..\n\
  \.#.##.#..#\n\
  \#.#.#.##.#\n\
  \....#...##\n\
  \...##..##.\n\
  \...#.#####\n\
  \.#.####.#.\n\
  \..#..###.#\n\
  \..##.#..#.\n\
  \\n\
  \Tile 1489:\n\
  \##.#.#....\n\
  \..##...#..\n\
  \.##..##...\n\
  \..#...#...\n\
  \#####...#.\n\
  \#..#.#.#.#\n\
  \...#.#.#..\n\
  \##.#...##.\n\
  \..##.##.##\n\
  \###.##.#..\n\
  \\n\
  \Tile 2473:\n\
  \#....####.\n\
  \#..#.##...\n\
  \#.##..#...\n\
  \######.#.#\n\
  \.#...#.#.#\n\
  \.#########\n\
  \.###.#..#.\n\
  \########.#\n\
  \##...##.#.\n\
  \..###.#.#.\n\
  \\n\
  \Tile 2971:\n\
  \..#.#....#\n\
  \#...###...\n\
  \#.#.###...\n\
  \##.##..#..\n\
  \.#####..##\n\
  \.#..####.#\n\
  \#..#.#..#.\n\
  \..####.###\n\
  \..#.#.###.\n\
  \...#.#.#.#\n\
  \\n\
  \Tile 2729:\n\
  \...#.#.#.#\n\
  \####.#....\n\
  \..#.#.....\n\
  \....#..#.#\n\
  \.##..##.#.\n\
  \.#.####...\n\
  \####.#.#..\n\
  \##.####...\n\
  \##..#.##..\n\
  \#.##...##.\n\
  \\n\
  \Tile 3079:\n\
  \#.#.#####.\n\
  \.#..######\n\
  \..#.......\n\
  \######....\n\
  \####.#..#.\n\
  \.#...#.##.\n\
  \#.#####.##\n\
  \..#.###...\n\
  \..#.......\n\
  \..#.###...\n\
  \"
