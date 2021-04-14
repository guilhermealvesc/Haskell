-- q1

figura :: Int -> Int -> Int -> Int -> Int -> String
figura l1 l2 l3 l4 ang 
  | (l1 == l2 && l3 == l4) || (l1 == l3 && l2 == l4) && ang == 90 = "retangulo"
  | l1 == l3 && l3 == l2 && l4 == l2 && ang == 90 = "quadrado"  
  | l1 == l3 && l3 == l2 && l4 == l2 && ang /= 90 = "losango"
  | otherwise = "simples"



-- q2
seleciona :: [[Int]] -> [Int]
seleciona lista = [h | (h:cab:_) <- lista, cab > 5]