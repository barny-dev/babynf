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
            else ([], rest)

rsplitWhenNot :: [a] -> (a -> Bool) -> ([a], [a])
rsplitWhenNot l matches =
    case l of
        [] -> ([], [])
        _ -> let rl = reverse l
                 (suffix, acc) = drainWhile rl [] matches
                 prefix = reverse acc
              in (prefix, suffix)

lrsplitWhenNot :: [a] -> (a -> Bool) -> ([a], [a], [a])
lrsplitWhenNot x matches =
    let (l, x') = lsplitWhenNot x matches
        (m, r) = rsplitWhenNot x' matches
     in (l, m, r)