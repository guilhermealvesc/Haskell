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

{- exercício 4 -}

{- exercício 5 -}
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

{- Exercício 8 -} 
serieOito :: Float -> Int -> Float
serieOito x 1 = 1/x;
serieOito x n 
  | mod n 2 == 0 = x/(fromIntegral n) + serieOito x (n-1)
  | otherwise = (fromIntegral n)/x + serieOito x (n-1)

{- exercício 9 -}
fizzAndBuzz :: Int -> String
fizzAndBuzz n
  | mod n 6 == 0 = "FizzBuzz"
  | mod n 2 == 0 = "Fizz"
  | mod n 3 == 0 = "Buzz"
  | otherwise = "No"

fizzbuzz :: Int -> [String]
fizzbuzz n = [fizzAndBuzz f | f <- [1..n]]

{- exercício 10 -}
sel_multiplos :: Int -> [Int] -> [Int]
sel_multiplos n lis = [num | num <-lis, mod num n == 0] 

{- exercício 11 -}
unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia n lis = length lis == ((length ([x | x <- lis, n /= x])) + 1)


{- exercício 12 -}
intercala :: [t] -> [t] -> [t]
intercala [] [] = []
intercala [] li = li
intercala li [] = li
intercala (x:xs) (h:hs) = (x:h:intercala xs hs)

{- exercício 13 -}
zipar :: [t] -> [t] -> [[t]]
zipar (x:[]) (h:hs) = [[x,h]]
zipar (x:xs) (h:[]) = [[x,h]]
zipar (h:hs) (x:xs) = [h,x] : zipar hs xs

{- exercício 14 -}
type Contato = (String, String, String, String)
-- recuperaNomeContato ::  -> String

{- exercício 15 -}
type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [("Rosa",1.66, 27,'F'),
    ("João", 1.85, 26, 'C'),
    ("Maria", 1.55, 62, 'S'),
    ("Jose", 1.78, 42, 'C'),
    ("Paulo", 1.93, 25, 'S'),
    ("Clara", 1.70, 33, 'C'),
    ("Bob", 1.45, 21, 'C'),
    ("Rosana", 1.58,39, 'S'),
    ("Daniel", 1.74, 72, 'S'),
    ("Jocileide", 1.69, 18, 'S')]

-- mais ineficiente
alturaMedia :: [Pessoa] -> Float
alturaMedia pes = (sum [alt | (_,alt,_,_) <- pes]) / fromIntegral (length pessoas)

maisNovaIdade :: [Pessoa] -> Int
maisNovaIdade [(_, _, idade, _)] = idade
maisNovaIdade ((nome1, altura1, idade1, sexo1) : (nome2, altura2, idade2, sexo2) : xs)
  | idade1 < idade2 = maisNovaIdade ((nome1, altura1, idade1, sexo1) : xs) 
  | otherwise = maisNovaIdade ((nome2, altura2, idade2, sexo2) : xs)

pessoaMaisVelha :: [Pessoa] -> (String, Char)
pessoaMaisVelha [(nome, _, _, ec)] = (nome, ec)
pessoaMaisVelha  ((nome1, altura1, idade1, sexo1) : (nome2, altura2, idade2, sexo2) : xs)
  | idade1 > idade2 = pessoaMaisVelha ((nome1, altura1, idade1, sexo1) : xs)
  | otherwise = pessoaMaisVelha ((nome2, altura2, idade2, sexo2) : xs)

maisVelhasCinquenta :: [Pessoa] -> [Pessoa]
maisVelhasCinquenta [] = []
maisVelhasCinquenta ((nome, altura, idade, ec) : pes)
  | idade >= 50 = (nome, altura, idade, ec) : maisVelhasCinquenta pes
  | otherwise = maisVelhasCinquenta pes

casadas :: Int -> [Pessoa] -> Int
casadas _ [] = 0
casadas i ((nome, altura, idade, ec):hs)
  | ec == 'C' && idade > i = 1 + casadas i hs
  | otherwise = casadas i hs

{- exercício 16 -}
insere_ord :: (Ord t) => t -> [t] -> [t]
insere_ord x [] = [x]
insere_ord x (h:hs) 
  | x < h = x:h:hs
  | otherwise = h : insere_ord x hs

{- exercício 17 -}
{- 
reverte :: [t] -> [t]
reverte [x] = [x]
reverte [x,y] = [y,x]
reverte (h:hs) 
  | 
  |  
-}

{- exercício 18 -}
-- elimina_repet :: [Int] -> [Int]


{- falta 14, 17, 18, 19, 20 -}