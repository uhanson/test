{-# LANGUAGE TupleSections #-}
module Main where
  import Data.Bifunctor
  import Data.List
  import Text.Printf
  
  ranges = map (\i -> (i, 0)) ([1..16] :: [Int])

  both f = bimap f f

  insert idx size = map shift
    where 
      shift t@(i, _) = if i >= idx then both (+1) t else t

  del idx size = filter (check . fst)
    where 
      check i = i < idx || i > idx + size - 1
  
  move from size to = map shift
    where
      (di, ds, ml, mr) = if from > to 
        then (to - from, size, to, from - 1)
        else (to - from - size, negate size, from + size, to - 1)
      shift t@(i, _) | i >= from && i <= from + size - 1    = both (+ di) t
                     | i >= ml && i <= mr                   = both (+ ds) t
                     | otherwise                            = t

  showElem (i, di) = printf "%3d -> %3d %3d" (i - di) i di 

  main :: IO ()
  main = do 
    let commands = move 11 4 3 . del 6 2 . move 5 4 13
        processed = sortOn fst (commands ranges)
    mapM_ (putStrLn . showElem) processed
