-- Dynamic Programming and Memoization
-- (http://jelv.is/blog/Lazy-Dynamic-Programming/)

import Data.Array ((!), listArray, range)
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Fibonacci
fib n = fibs !! n
  where
    fibs = 0 : 1: zipWith (+) fibs (drop 1 fibs)

-- not very efficient, for illustration only
fib' max = go max
  where
    go 0 = 0
    go 1 = 1
    go n = fibs ! (n - 1) + fibs ! (n - 2)
    fibs = listArray (0, max) [go x | x <- [0..max]]


-- String Edit Distance
naive a b = d (length a) (length b)
  where d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) == b !! (j - 1) = d (i - 1) (j - 1)
          | otherwise = minimum [ d (i - 1) j + 1
                                , d i (j - 1) + 1
                                , d (i - 1) (j - 1) + 1
                                ]
-- example
-- naive "kitten" "sitting" == 3
-- naive "aaaaaaaaaa" "bbbbbbbbbb" == 3  which takes a long time to calculate: (97.46 secs, 69,400,860,248 bytes)

basic a b = d m n
  where (m, n) = (length a, length b)
        d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) == b !! (j - 1) = ds ! (i -1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j) + 1
                                , ds ! (i, j - 1) + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]
        ds = listArray bounds [ d i j | (i, j) <- range bounds]
        bounds = ((0, 0), (m, n))
-- basic "aaaaaaaaaa" "bbbbbbbbbb" == 3  which takes a long time to calculate: (0.00 secs, 616,440 bytes)

better a b = d m n
  where (m, n) = (length a, length b)
        arr = listArray (1, m) a
        brr = listArray (1, n) b

        d i 0 = i
        d 0 j = j
        d i j
          | arr ! i == brr ! j = ds ! (i -1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j) + 1
                                , ds ! (i, j - 1) + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]
        ds = listArray bounds [ d i j | (i, j) <- range bounds]
        bounds = ((0, 0), (m, n))

-- creating actual editing steps
data Action = NoChange | Add Char | Remove Char | Modify Char Char

instance Show Action where
  show NoChange = "No change"
  show (Add c) = "Insert " ++ show c
  show (Remove c) = "Delete " ++ show c
  show (Modify c1 c2) = "Change " ++ show c1 ++ " to " ++ show c2

type Distance = Int

cost :: Action -> Distance
cost NoChange = 0
cost _ = 1

script :: (Action -> Distance) -> [Char] -> [Char] -> [Action]
script cost a b = reverse . snd $ d m n
  where (m, n) = (length a, length b)
        arr = listArray (1, m) a
        brr = listArray (1, n) b

        d :: Int -> Int -> (Distance, [Action])
        d 0 0 = (0, [])
        d i 0 = go (i - 1) 0 $ Remove (arr ! i)
        d 0 j = go 0 (j - 1) $ Add (brr ! j)
        d i j
          | arr ! i == brr ! j = go (i - 1) (j - 1) NoChange
          | otherwise = minimum' [ go (i - 1) j $ Remove (arr ! i)
                                 , go i (j - 1) $ Add (brr ! j)
                                 , go (i - 1) (j - 1) $ Modify (arr ! i) (brr ! j)
                                 ]
        minimum' = minimumBy (comparing fst)
        go i j action = let (score, actions) = ds ! (i, j)
                        in (score + cost action, action:actions)
        ds = listArray bounds [d i j | (i, j) <- range bounds]
        bounds = ((0, 0), (m, n))
