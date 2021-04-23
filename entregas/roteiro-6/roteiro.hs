{- Exercício 1 -}
-- a
type Data = (Int, Int, Int)
valida :: Data -> Bool
valida (dia, mes, ano) 
  | (dia < 1 || dia > 31) || (mes < 1 || mes > 12) = False {- Tira mes e dia com valores inválidos -}
  | dia > 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = False {- Tira as datas com dia 31 em meses que são de 30 dias -}
  | mes == 2 && dia > 29 = False {- Tira as datas com dia 30 de fevereiro-}
  | mes == 2 && dia > 28 && not (bissexto ano) = False {- Tira as datas com dia 29 que não são bissextas -}
  | otherwise = True
  where
    bissexto :: Int -> Bool
    bissexto ano 
      | mod ano 400 == 0 = True
      | mod ano 4 == 0 && mod ano 100 /= 0 = True
      | otherwise = False

-- b
bissextos :: [Int] -> [Int]
bissextos xs = [x | x <- xs, bissexto x]
  where
    bissexto :: Int -> Bool
    bissexto ano 
      | mod ano 400 == 0 = True
      | mod ano 4 == 0 && mod ano 100 /= 0 = True
      | otherwise = False

--c 
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo :: Emprestimos
bdEmprestimo = 
  [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),   
  ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),   
  ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]
atrasados :: Emprestimos -> Data -> Emprestimos
atrasados livros dataAtual = [(codigoLivro, codigoAluno, dataEmprestimo, dataDevolucao, status) | (codigoLivro, codigoAluno, dataEmprestimo, dataDevolucao, status) <- livros, not (precede dataAtual dataDevolucao)]
  where
    precede :: Data -> Data -> Bool
    precede (dia1, mes1, ano1) (dia2, mes2, ano2) 
      | not (valida (dia1, mes1, ano1)) || not (valida (dia2, mes2, ano2)) = False
      | ano1 < ano2 = True
      | ano1 == ano2 && mes1 < mes2 = True
      | ano1 == ano2 && mes1 == mes2 && dia1 < dia2 = True
      | otherwise = False
      where
        valida :: Data -> Bool
        valida (dia, mes, ano) 
          | (dia < 1 || dia > 31) || (mes < 1 || mes > 12) = False {- Tira mes e dia com valores inválidos -}
          | dia > 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = False {- Tira as datas com dia 31 em meses que são de 30 dias -}
          | mes == 2 && dia > 29 = False {- Tira as datas com dia 30 de fevereiro-}
          | mes == 2 && dia > 28 && not (bissexto2 (dia, mes, ano)) = False {- Tira as datas com dia 29 que não são bissextas -}
          | otherwise = True
          where
            bissexto2 :: Data -> Bool
            bissexto2 (dia, mes, ano)
              | mod ano 400 == 0 = True
              | mod ano 4 == 0 && mod ano 100 /= 0 = True
              | otherwise = False

--d
type Par = (Int, Int)
fibo2 :: Int -> Int
fibo2 n = fst(n_par(n))
  where
    n_par :: Int -> Par
    n_par n = n_calcula (n, (1,1))
      where
        n_calcula :: (Int, Par) -> Par
        n_calcula (n, par)
          | n == 1 = par
          | otherwise = n_calcula (n - 1, passo(par))
          where
            passo :: Par -> Par
            passo (x, y) = (y, x + y)

-- e
fatorial :: Int -> Int
fatorial n
  | n == 0 = 1
  | otherwise = prodIntervalo(1, n)
  where
    prodIntervalo :: (Int, Int) -> Int
    prodIntervalo (m, n)
      | m > n = -1
      | m == n = n
      | otherwise = n * prodIntervalo(m, n - 1)

{- Exercício 2 -}

{- Exercício 3 -}

{- Exercício 4 -}

{- Exercício 5 -}