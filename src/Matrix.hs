{-# LANGUAGE TypeFamilies #-}
module Matrix where

import qualified Data.List
import Numeric.Natural (Natural)
import Prelude hiding (exp)
type Column a = [a]
type Row a = [a]

newtype Matrix a = Matrix {rows :: Column (Row a)} deriving (Eq)

-- | Ensure that the matrix rows have equal length
fromLists :: [[a]] -> Maybe (Matrix a)
fromLists [] = Nothing
fromLists list
    | all (l ==) ls = Just (Matrix list)
    | otherwise = Nothing
    where
        (l:ls) = length <$> list

-- | Tranpose the matrix
transpose :: Matrix a -> Matrix a
transpose = Matrix . Data.List.transpose . rows

-- | Report all the columns of the matrix
columns :: Matrix a -> Row (Column a)
columns = rows . transpose

-- | The dimensions of the matrix
dim :: Matrix a -> (Natural, Natural)
dim (Matrix x) = (fromIntegral $ length x, fromIntegral $ length $ head x)

instance Functor Matrix where
  fmap f (Matrix xs) = Matrix $ map (map f) xs

-- | The inner and outer operators of a product 
-- >>> fuse (*) (+) [(1,2), (3,4)]
-- 14

fuse :: (a -> b -> c) -> (c -> c -> c) -> [(a,b)] -> c
fuse f g = foldl1 g . fmap (uncurry f)

-- | The inner product of two matricies
dot :: ([(a,b)] -> c) -> Matrix a -> Matrix b -> Maybe (Matrix c)
dot inner lhs rhs
    | compatible = fromLists $ rows lhs * columns rhs
    | otherwise = Nothing
    where
        compatible = length (rows lhs) == length (columns rhs)
        lhs * rhs = [[inner $ zip x y | x <- lhs] | y <- rhs ]

-- | Binary iteration in O(log2(n)), but only if the operator is 
-- >>> binIter (**) 2 !! 4

binIter :: (a -> a -> a) -> a -> [a]
binIter f m = exp' [m, f m m]
  where
    exp' (m1:m2:ms) = m1 : exp' ((m2:ms) ++ [f m1 m2, f m2 m2])

-- | The hadamard product of two matricies
hadamard :: (a -> b -> c) -> Matrix a -> Matrix b -> Maybe (Matrix c)
hadamard f lhs rhs
    | compatible = fromLists $ zipWith (zipWith f) (rows lhs) (rows rhs)
    | otherwise = Nothing
    where
        compatible = dim lhs == dim rhs
    
instance Show a => Show (Matrix a) where
  show m = Data.List.intercalate "\n" $ wrap . unwords <$> rows padded
    where
        pad n s = replicate n ' ' ++ s
        wrap s = "(" ++ s ++ ")"

        strings = show <$> m

        lengths = length <$> strings

        widths = Matrix
          $ replicate (length $ rows lengths)
          $ fmap maximum <$> columns
          $ lengths

        Just missing = hadamard (-) widths lengths
        Just padded = hadamard pad missing strings

