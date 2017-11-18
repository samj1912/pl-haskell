makeset [] = []
makeset set@(x:xs) 
    | x `elem` y = y
    | otherwise = x:y
    where y = makeset xs
empty x = not(null x)    
union x y = makeset(x ++ y)
intersect x y = let new_x = [e | e <- x, e `elem` y] in makeset [e | e <-y, e `elem` new_x]
subt x y = let ins = intersect x y in [e | e <- x, not (e `elem` ins)]