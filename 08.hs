compress :: Eq a => [a] -> [a]
compress = foldr (\x acc -> if x `elem` acc then acc else x : acc) []