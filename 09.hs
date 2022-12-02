pack :: Eq a => [a] -> [[a]]
pack = reverse . (\acc -> snd acc : fst acc) . fold ([], [])
    where
        fold = foldl match
        match (acc, []) x = (acc, [x])
        match (acc, y:ys) x
            | y == x = (acc, x:y:ys)
            | otherwise = ((y:ys):acc, [x])