data Encoded a
  = Multiple Int a
  | Single a
  deriving (Show)

decodeModified :: [Encoded a] -> [a]
decodeModified [] = []
decodeModified ((Multiple c x):xs) = replicate c x ++ decodeModified xs
decodeModified ((Single x):xs) = x : decodeModified xs
