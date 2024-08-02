module Data.BAByNF.Util.List where



drainOnce :: [a] -> [a] -> ([a], [a])
drainOnce from to =
    case from of
        [] -> (from, to)
        x:xs -> (xs, x:to)

drainIf :: [a] -> [a] -> (a -> Bool) -> Maybe ([a], [a])
drainIf from to cond = 
    case from of
        [] -> Nothing
        x:xs -> 
            if cond x 
                then Just (xs, x:to)
                else Nothing

drainWhile :: [a] -> [a] -> (a -> Bool) -> ([a], [a])
drainWhile from to cond =
    case drainIf from to cond of
        Just (from', to') -> drainWhile from' to' cond
        Nothing -> (from, to)

lsplitWhenNot :: [a] -> (a -> Bool) -> ([a], [a])
lsplitWhenNot l matches =
    case l of 
        [] -> ([], []) 
        (focus:rest) ->
            if matches focus
                then let (prefixTail, suffix) = lsplitWhenNot rest matches
                    in (focus:prefixTail, suffix)
            else ([], l)

rsplitWhenNot :: [a] -> (a -> Bool) -> ([a], [a])
rsplitWhenNot l matches = foldr fn ([], []) l 
    where fn x acc = case acc of
            ([], back) | matches x -> ([], x:back)  
                       | otherwise -> ([x], back)
            (front, back) -> (x:front, back)
rstrip :: [a] -> (a -> Bool) -> [a]
rstrip l matches = foldr fn [] l
    where fn x acc = case acc of
            [] | matches x -> []
               | otherwise -> [x]
            _ -> x:acc

lstrip :: [a] -> (a -> Bool) -> [a]
lstrip l matches = case l of
    [] -> []
    x:xs | matches x -> lstrip xs matches
         | otherwise -> l

lrsplitWhenNot :: Show a => [a] -> (a -> Bool) -> ([a], [a], [a])
lrsplitWhenNot x matches =
    let (l, x') = lsplitWhenNot x matches
        (m, r) = rsplitWhenNot x' matches
     in (l, m, r)