import Data.List

-- Normal Reduce Function
rd :: (a -> a -> a) -> [a] -> a
rd _ [] = error "the list to be reduced should not be empty"
rd f (x:xs) = rdHelper x xs
    where rdHelper a [] = a
          rdHelper a (y:ys) = rdHelper (f a y) ys

-- Sort by the key
sortByKey :: Ord k => [(k, v)] -> [(k, v)]
sortByKey lst = sortBy keyCmp lst
    where keyCmp x y = compare (fst x) (fst y)

-- Reduce by Key
rdByKey :: Eq k => (v -> v -> v) -> [(k,v)] -> [(k,v)]
rdByKey _ [] = []
rdByKey _ (x:[]) = [x]
rdByKey f (x:y:xs) = if sameKey x y then rdByKey f ([(combinePair f x y)] ++ xs) else ([x] ++ (rdByKey f ([y] ++ xs)))
    where combinePair f x y = (fst x, f (snd x) (snd y)) 
          sameKey (k1, v1) (k2, v2) = k1 == k2

-- Group By the Key in key-value 
groupByKey :: Ord k => [(k, v)] -> [(k, [v])]                  
groupByKey = rdByKey (++) . sortByKey . map (\x -> (fst x, [snd x]))

-- Using the function to create the key, group it, and abandon the key
groupByMapping :: Ord k => (a -> k) -> [a] -> [[a]]
groupByMapping f = map snd . groupByKey . map (\x -> (f x, x))
