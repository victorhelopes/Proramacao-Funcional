--1
lst1 = [x*2 | x <- [1..10], x*2 >= 12]
--[12,14,16,18,20]
lst2 = [ x | x <- [50..100], mod x 7 == 3]
--[52,59,66,73,80,87,94]
lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
--[10,11,12,14,16,17,18,20]
lst4=[(x,y)| x <- [1..4], y <- [x..5]]
--[(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)]

--2
quadrados x y = [ n**2 | n<-[x..y] ]

--3
seleciona_impares l = [ x | x<-l, mod x 2 == 1]

--4
tabuada :: Int -> [Int]
tabuada x = [ a*n | a<-[1,2..10], n<-[x] ] 

--5
bissexto :: Int -> Bool
bissexto x = if(mod x 4 == 0 && not(mod x 100 == 0) || (mod x 400 == 0)) then True
                else False

bissextos l = [ x | x<-l, bissexto x == True]

--6 terminar
sublistas :: [[a]] -> [a]
sublistas (x:xs) = [ n | a<-(x:xs), n<-a ]

--7
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =[("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
               ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
               ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]
    
atrasados::[Emprestimo]->Data->[Emprestimo]
atrasados l (dia,mes,ano) = [ x | x<-l, verifica x (dia,mes,ano) == True]

verifica:: Emprestimo->Data->Bool
verifica (a, b, dia_emp, (diaentrega,mesentrega,anoentrega), e) (dia,mes,ano) | ano > anoentrega = True
                                                                              | mes > mesentrega = True
                                                                              | dia > diaentrega = True
                                                                              | otherwise = False

--8
n_pares:: [Int]->Int
n_pares [] = 0
n_pares (x:xs)|mod x 2 == 0 = 1 + n_pares xs
              |otherwise = n_pares xs

--9
produtorio:: [Int]->Int
produtorio [] = 0
produtorio [x] = x
produtorio (x:xs) = x * produtorio xs

--10
comprime::[[Int]]->[Int]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

--11
tamanho::[Int]->Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

--12
uniaoRec :: [Int] -> [Int] -> [Int]
uniaoRec a b = a ++ [b | x<-b, b<-verificaElem a x]

--13
uniaoRec2::[Int]->[Int]->[Int]
uniaoRec2 a [] = a
uniaoRec2 a (x:xs) | verificaElem a x == [x] = uniaoRec2 (a++[x]) xs 
                   | otherwise = uniaoRec2 a xs

verificaElem::[Int]-> Int->[Int]
verificaElem [] a = [a]
verificaElem (x:xs) a = if (a == x) then [] else verificaElem xs a