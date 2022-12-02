data Encoded a
  = Multiple Int a
  | Single a
  deriving (Show)

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = foldr fold []
  where
    fold x [] = [Single x]
    fold x ((Single y):ys)
      | x == y = Multiple 2 x : ys
      | otherwise = Single x : Single y : ys
    fold x ((Multiple c y):ys)
      | x == y = Multiple (c + 1) x : ys
      | otherwise = Single x : Multiple c y : ys
