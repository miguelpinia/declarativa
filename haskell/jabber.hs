{-# LANGUAGE RankNTypes #-}
import Test.QuickCheck
import Test.HUnit
import Data.List


jabber :: IO ()
jabber = do
  wocky <- readFile "jabberwocky.txt"
  let wockylines = drop 2 (lines wocky)  -- quita el titulo
  count <- printFirstLines wockylines
  putStrLn $ "Hay " ++ show count ++ " estrofas en el Jabberwocky."

printFirstLines :: [String] -> IO Int
printFirstLines ls = do
  let first_lines = extractFirstLines ls
  putStr (unlines first_lines)
  return $ length first_lines

extractFirstLines :: [String] -> [String]
extractFirstLines []         = []
extractFirstLines [_]        = []
extractFirstLines ("" : first : rest)
  = first : extractFirstLines rest
extractFirstLines (_ : rest) = extractFirstLines rest



data Tree a = Node (Tree a) a (Tree a)
            | Empty
              deriving (Show)

zipTree1 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree1 _ (Node _ _ _) Empty = Nothing
zipTree1 _ Empty (Node _ _ _) = Nothing
zipTree1 _ Empty Empty        = Just Empty
zipTree1 f (Node l1 x r1) (Node l2 y r2) =
    case zipTree1 f l1 l2 of
      Nothing -> Nothing
      Just l  -> case zipTree1 f r1 r2 of
                   Nothing -> Nothing
                   Just r  -> Just $ Node l (f x y) r


bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe mx f = case mx of
                   Nothing -> Nothing
                   Just x  -> f x

zipTree2 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree2 _ (Node _ _ _) Empty = Nothing
zipTree2 _ Empty (Node _ _ _) = Nothing
zipTree2 _ Empty Empty        = Just Empty
zipTree2 f (Node l1 x r1) (Node l2 y r2) =
    bindMaybe (zipTree2 f l1 l2) $ \l ->
        bindMaybe (zipTree2 f r1 r2) $ \r ->
            Just (Node l (f x y) r)


zipTree3 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree3 _ (Node _ _ _) Empty = Nothing
zipTree3 _ Empty (Node _ _ _) = Nothing
zipTree3 _ Empty Empty        = Just Empty
zipTree3 f (Node l1 x r1) (Node l2 y r2) =
    zipTree3 f l1 l2 >>= \l ->
        zipTree3 f r1 r2 >>= \r ->
            return (Node l (f x y) r)


addM :: Monad m => m Int -> m Int -> m Int
addM mx my = do
  x <- mx
  y <- my
  return $ x + y

addM' :: Monad m => m Int -> m Int -> m Int
addM' mx my = mx >>= \x -> my >>= \y -> return (x + y)

zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ (Node _ _ _) Empty = Nothing
zipTree _ Empty (Node _ _ _) = Nothing
zipTree _ Empty Empty        = Just Empty
zipTree f (Node l1 x r1) (Node l2 y r2) = do
    l <- zipTree f l1 l2
    r <- zipTree f r1 r2
    return $ Node l (f x y) r

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

ex01 = return 7 >>= check >>= halve
ex02 = return 12 >>= check >>= halve
ex03 = return 12 >>= halve >>= check

ex04 = do
  checked <- check 7
  halve checked
ex05 = do
  checked <- check 12
  halve checked
ex06 = do
  halved <- halve 12
  check halved

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex07 = [10,20,30] >>= addOneOrTwo
ex08 = do
  num <- [10, 20, 30]
  addOneOrTwo num

merge1 :: Ord a => [a] -> [a] -> [a]
merge1 (x:xs) (y:ys)
  | x < y     = x : merge1 xs ys
  | otherwise = y : merge1 xs ys
merge1 _      _      = []

test1_merge1 :: Test
test1_merge1 = "alternating numbers: [1,3,5] [2,4,6]" ~:
               merge1 [1,3,5] [2,4,6] ~?= [1,2,3,4,5,6]

test2_merge1 :: Test
test2_merge1 = TestList ["one element: [1] []" ~:
                          merge1 [1] [] ~?= [1],
                          test1_merge1]

type MergeFun = Ord a => [a] -> [a] -> [a]
test2 :: MergeFun -> Test
test2 merge = TestList [ "one element: [1] []" ~:
                         merge [1] [] ~?= [1],
                         "alternating numbers: [1,3,5] [2,4,6]" ~:
                         merge [1,3,5] [2,4,6] ~?= [1,2,3,4,5,6]
                       ]

merge2 :: MergeFun
merge2 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge2 xs all_ys
  | otherwise = y : merge2 all_xs ys
merge2 _             _             = []

-- λ> runTestTT (test2 merge2)

merge3 :: MergeFun
merge3 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge3 all_ys xs
  | otherwise = y : merge3 all_xs ys
merge3 xs            []            = xs
merge3 _             _             = []

-- Agreguemos otro test:

-- test3 :: MergeFun -> Test
-- test3 merge = "empty lists: [] []" ~:
--               merge [] [] ~?= []

test3 :: MergeFun -> Test
test3 merge = "empty lists: [] []" ~:
              merge [] [] ~?= ([] :: [Integer])

-- λ> runTestTT (test2 merge3)

prop_numElements_merge3 :: [Integer] -> [Integer] -> Bool
prop_numElements_merge3 xs ys = length xs + length ys == length (merge3 xs ys)

-- λ> quickCheck prop_numElements_merge3


prop_numElements :: MergeFun -> [Integer] -> [Integer] -> Bool
prop_numElements merge xs ys
  = length xs + length ys == length (merge xs ys)

  -- E intentamos otra vez con nuesta función

merge4 :: MergeFun
merge4 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge4 xs all_ys
  | otherwise = y : merge4 all_xs ys
merge4 xs            ys            = xs ++ ys

-- λ> quickCheck (prop_numElements merge4)

prop_sorted1 :: MergeFun -> [Integer] -> [Integer] -> Bool
prop_sorted1 merge xs ys  = merge xs ys == sort (xs ++ ys)
-- *Main> quickCheck (prop_sorted1 merge4)


prop_sorted2 :: MergeFun -> [Integer] -> [Integer] -> Property
prop_sorted2 merge xs ys  = isSorted xs && isSorted ys ==> merge xs ys == sort (xs ++ ys)
isSorted :: Ord a => [a] -> Bool
isSorted (a:b:rest) = a <= b && isSorted (b:rest)
isSorted _          = True
-- *Main> quickCheck (prop_sorted2 merge4)
