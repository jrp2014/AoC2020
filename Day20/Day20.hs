module Day20 where

-- https://github.com/MatthiasCoppens/AOC2020/blob/master/day20/solution.hs
import Data.List (transpose)
import Data.List.Split

type Grid = [[Char]]

data Tile = Tile {tileID :: Int, pixels :: Grid}
  deriving (Show)

parseTile :: String -> Tile
parseTile s = Tile (read . init . last $ words idLine) tile
  where
    (idLine : tile) = lines s -- NB may neeed to remove trailing empty line

parseTiles :: String -> [Tile]
parseTiles = map parseTile . splitOn "\n\n"

-- include flipped edges
getEdges :: Tile -> Tile
getEdges Tile {tileID = tileID, pixels = pixels} =
  Tile
    { tileID = tileID,
      pixels = edges ++ map reverse edges
    }
  where
    edges = [head pixels, last pixels, head pixels', last pixels']
    pixels' = transpose pixels

part1 :: String -> Int
-- take the first 4 tiles that have two edges in commpn with other tiles
part1 = product . take 4 . go . map getEdges . parseTiles
  where
    go :: [Tile] -> [Int]
    go (tile@Tile {tileID = n, pixels = edges} : rest)
      -- retain the tileID of the tile if it has an edge in common
      -- with the edges of up to 2 of the rest of the tiles (whether normal or reversed)
      | null $ drop 2 $ filter (any (`elem` edges)) $ map pixels rest =
        n : go (rest ++ [tile])
      | otherwise = go (rest ++ [tile]) -- cycle through the list

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input

ex1 :: String
ex1 =
  "Tile 2311:\n\
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
