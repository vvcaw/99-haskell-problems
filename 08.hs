compress :: Eq a => [a] -> [a]
compress = foldr (\x acc -> if null acc || head acc /= x then x : acc else acc) []