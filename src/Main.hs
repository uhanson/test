{-# LANGUAGE TupleSections #-}
module Main where
  import Data.Bifunctor
  import Text.Printf

  type Idx = Int
  type Size = Int
  type Delta = Int

  data Command
    = Insert Idx Size
    | Delete Idx Size
    | Move Idx Size Idx 
    deriving (Eq, Show)

  data Step
    = Shift Idx Size Delta
    | Del Idx Size
    deriving (Eq, Show)

  data I = I !Delta !Bool
    deriving (Eq, Show)
  
  ranges = map (\i -> (i, 0)) ([1..16] :: [Integer])

  both f = bimap f f

  insert idx size = map shift
    where 
      shift t@(i, _) = if i >= idx then both (+1) t else t

  delete idx size = filter del
    where 
      del (i, _) = i < idx || i > idx + size - 1
  
  move from size to = map shift
    where
      (ds, ml, mr) = if from > to 
        then (size, to, from - 1)
        else (negate size, from + size, to - 1)
      di = to - from + ds
      shift t@(i, _) | i >= from && i <= from + size - 1    = both (+ di) t
                     | i >= ml && i <= mr                   = both (+ ds) t
                     | otherwise                            = t

  showElem (i, di) = printf "%3d -> %3d %3d" (i - di) i di 

  main :: IO ()
  main = do 
    let commands = move 11 4 3 -- . delete 6 2 . move 5 4 13
        processed = commands ranges
    mapM_ (putStrLn . showElem) processed
