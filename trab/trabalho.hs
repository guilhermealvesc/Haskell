{- Exercício 1 -}
analisa_raizes :: Float -> Float -> Float -> String
analisa_raizes 0 b c = "4-equacao degenerada"
analisa_raizes a b c 
  | b * b > 4 * a * c = "1-possui duas raizes reais"
  | b * b == 4 * a * c = "2-possui uma raiz real" 
  | otherwise = "3-nunhuma raiz real"


{- Exercício 2 -}
equacao :: Float -> Float -> Float -> (Float, Float)
equacao a b c 
  | a /= 0 = ((-b + sqrt(b * b - 4 * a * c)) / (2 * a), (-b - sqrt(b * b - 4 * a * c)) / (2 * a))
  | otherwise = (-c / b, a)


{- Exercício 3 -}
type Data = (Int, Int, Int)

-- Função auxiliar para ver se ano é bissexto
bissexto2 :: Data -> Bool
bissexto2 (dia, mes, ano)
  | mod ano 400 == 0 = True
  | mod ano 4 == 0 && mod ano 100 /= 0 = True
  | otherwise = False

-- Função auxiliar para ver se data é valida
valida :: Data -> Bool
valida (dia, mes, ano) 
  | (dia < 1 || dia > 31) || (mes < 1 || mes > 12) = False {- Tira mes e dia com valores inválidos -}
  | dia > 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = False {- Tira as datas com dia 31 em meses que são de 30 dias -}
  | mes == 2 && dia > 29 = False {- Tira as datas com dia 30 de fevereiro-}
  | mes == 2 && dia > 28 && not (bissexto2 (dia, mes, ano)) = False {- Tira as datas com dia 29 que não são bissextas -}
  | otherwise = True

-- Função auxiliar para ver se data precede outra data
precede :: Data -> Data -> Bool
precede (dia1, mes1, ano1) (dia2, mes2, ano2) 
  | not (valida (dia1, mes1, ano1)) || not (valida (dia2, mes2, ano2)) = False
  | ano1 < ano2 = True
  | ano1 == ano2 && mes1 < mes2 = True
  | ano1 == ano2 && mes1 == mes2 && dia1 < dia2 = True
  | otherwise = False

-- Função auxiliar para calcular idade
calculaIdade :: Data -> Data -> Int
calculaIdade (diaA, mesA, anoA) (diaN, mesN, anoN)
  | precede (diaN, mesN, anoN) (diaA, mesA, anoA) = 0
  | anoA == anoN = 0
  | (mesA > mesN) || (mesA == mesN && diaA >= diaN) = anoA - anoN
  | otherwise = anoA - anoN - 1

precoPassagem :: Float -> Data -> Data -> Float
precoPassagem preco dataAtual dataNascimento 
  | calculaIdade dataAtual dataNascimento < 2 = preco * 0.15
  | calculaIdade dataAtual dataNascimento <= 10 = preco * 0.4
  | calculaIdade dataAtual dataNascimento >= 70 = preco * 0.5
  | otherwise = preco


{- Exercício 5 -}
-- (a)
contaNegM2 :: [Int] -> Int
contaNegM2 lst = length [x | x <- lst, x >= 0, mod x 3 == 0]

-- (b)
listaNegM2 :: [Int] -> [Int]
listaNegM2 lst = [x | x <- lst, x >= 0, mod x 3 == 0]


{- Exercício 6 -}
fatores :: Int -> [Int]
fatores n = [x | x <- [1..n], mod n x == 0]

primos :: Int -> Int -> [Int]
primos a b = [x | x <- [a + 1 .. b - 1], fatores x == [1,x]]

{- Exercício 8 deu troios -} 
serieOito :: Float -> Int -> Float
serieOito x n = sum(take n [1/x, x/2 ..])