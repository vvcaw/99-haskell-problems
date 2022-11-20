myButLast :: [a] -> a
myButLast [] = error "No but last element in list with no elements!"
myButLast [a] = error "No but last element in list with one element!"
myButLast [a, _] = a
myButLast (x:xs) = myButLast xs 