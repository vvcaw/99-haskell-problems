elementAt :: [a] -> Int -> a
elementAt [] 1 = error "Can't find element in list!"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)
elementAt _ _ = error "Invalid index!"
