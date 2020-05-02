-- A datatype for rectangular (i.e. un-jagged) matrices.

module Matrix (Matrix, zipMatrix, render, transpose, reflVert, reflHoriz,
               rotLeft, rotRight, toLists, outerProduct, indexMatrix,
               matrix) where

import Control.Monad
import qualified Data.List as List

data Matrix a = Matrix [[a]]
  deriving Eq

instance Functor Matrix where
  fmap f (Matrix m) = Matrix $ (fmap f) <$> m

-- Traverses along rows.
-- Call transpose first if you want to traverse along columns.
instance Foldable Matrix where
  foldr f z (Matrix m) = foldr (flip $ foldr f) z m
--foldr f z (Matrix m) = foldr f z $ concat m

instance Traversable Matrix where
  traverse f (Matrix m) = Matrix <$> traverse (traverse f) m

instance Show a => Show (Matrix a) where
  show (Matrix m) = unlines $ unwords . fmap show <$> m

zipMatrix (Matrix m1) (Matrix m2) = Matrix $ uncurry zip <$> zip m1 m2

render (Matrix m) = putStr $ unlines m

transpose (Matrix m) = Matrix $ List.transpose m

reflVert  (Matrix m) = Matrix $ reverse  $  m
reflHoriz (Matrix m) = Matrix $ reverse <$> m

rotLeft  = reflVert  . transpose
rotRight = reflHoriz . transpose

toLists (Matrix m) = m

outerProduct f xs ys = Matrix $ flip fmap ys <$> f <$> xs

indexMatrix n m = transpose $ outerProduct (,) [0..(m-1)] [0..(n-1)]

matrix n m f = uncurry f <$> indexMatrix n m
