module Main where
  import Data.Array.IO

  data Step a
    = Move a a a
    | Del a a
    deriving (Eq, Show)

  ranges :: IO (IOUArray Int Int)
  ranges = newListArray (1, 256) [1..]

  main :: IO ()
  main = do
    r <- ranges
    print r
