-- (h:t)
-- heat tail

contElem :: [a] -> Int
contElem [] = 0;
contElem (_:t) = 1 + contElem(t)