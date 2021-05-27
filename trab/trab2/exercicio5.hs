l1 = [1 .. 2000]

l2 = [2000, 1999 .. 1]

l3 = l1 ++ [0]

l4 = 0 : l2

l5 = l1 ++ [0] ++ l2

l6 = l2 ++ [0] ++ l1

l7 = l2 ++ [0] ++ l2

x1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

x2 = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

x3 = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

x4 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11]

x5 = [11, 12, 13, 14, 15, 5, 4, 3, 2, 1, 16, 17, 18, 19, 20, 10, 9, 8, 7, 6]

x6 = [1, 12, 3, 14, 5, 15, 4, 13, 2, 11, 6, 17, 8, 19, 20, 10, 9, 18, 7, 16]

x7 = [20, 8, 2, 11, 13, 3, 7, 18, 14, 4, 16, 10, 15, 1, 9, 17, 19, 12, 5, 6]

separaListas :: Ord a => [a] -> ([a], [a])
separaListas list = splitAt (div (length list) 2) list

merge :: Ord a => [a] -> [a] -> ([a], Int)
merge xs [] = (xs, 0)
merge [] ys = (ys, 0)
merge (x : xs) (y : ys)
  | x <= y = (x : l1, cont1 + 1)
  | otherwise = (y : l2, cont2 + 1)
  where
    (l1, cont1) = merge xs (y : ys)
    (l2, cont2) = merge (x : xs) ys

mergeSort :: Ord a => [a] -> ([a], Int)
mergeSort [] = ([], 0)
mergeSort [x] = ([x], 0)
mergeSort list = (merged, cont + contEsq + contDir)
  where
    (esq, dir) = separaListas list
    (newEsq, contEsq) = mergeSort esq
    (newDir, contDir) = mergeSort dir
    (merged, cont) = merge newEsq newDir
