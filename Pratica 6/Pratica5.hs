--1
--a)
valida dd mm aaaa | dd > 31 || mm > 12 = False
                  | (mm == 4 || mm == 6 || mm == 9 || mm == 11) && dd == 31 = False
                  | mm == 2 && dd >28 && (ano_bissexto aaaa == False) = False
                  |otherwise = True
                  where ano_bissexto aaaa = bissexto aaaa
--b)
bissexto :: Int -> Bool
bissexto x = if(mod x 4 == 0 && not(mod x 100 == 0) || (mod x 400 == 0)) then True
                else False

bissextos l = [ x | x <-l, verifica_ano x == True] where verifica_ano a = bissexto a   

--c)
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =[("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
               ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
               ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]
    
atrasados::[Emprestimo]->Data->[Emprestimo]
atrasados l (dia,mes,ano) = [ x | x<-l, f x == True] where f x = verifica x (dia,mes,ano)

verifica:: Emprestimo->Data->Bool
verifica (_ , _, _, (diaentrega,mesentrega,anoentrega), _) (dia,mes,ano) | ano > anoentrega = True
                                                                         | mes > mesentrega && ano == anoentrega = True
                                                                         | dia > diaentrega && mes == mesentrega && ano == anoentrega = True
                                                                         | otherwise = False

--d)
passo:: Int->(Int,Int) -> Int
passo 0 (x,y) = x
passo n (x,y)= passo (n-1) (y, x+y)

fibo1:: Int -> Int
fibo1 n = aux n  where aux n = passo n (0,1) 

--e)
prodintervalo::Int->Int->Int
prodintervalo m n = if(n < m) then 1
                    else m * prodintervalo (m+1) n

fatorial1::Int->Int
fatorial1 n = f 1 n where f m n = prodintervalo m n

--2
--a)
valida2 dd mm aaaa | dd > 31 || mm > 12 = False
                   | (mm == 4 || mm == 6 || mm == 9 || mm == 11) && dd == 31 = False
                   | mm == 2 && dd >28 && (let ano = aaaa in bissexto ano == False) = False
                   |otherwise = True

--b)
bissexto2 :: Int -> Bool
bissexto2 x = if(mod x 4 == 0 && not(mod x 100 == 0) || (mod x 400 == 0)) then True
                else False

bissextos2 l = [ x | x <-l, let ano = x in bissexto2 ano == True]  

--c)
bdEmprestimo2::Emprestimos
bdEmprestimo2 =[("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
               ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
               ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]
    
atrasados2::[Emprestimo]->Data->[Emprestimo]
atrasados2 l (dia,mes,ano) = [ x | x<-l, let a = x in verifica a (dia,mes,ano)  == True]

verifica2:: Emprestimo->Data->Bool
verifica2 (_ , _, _, (diaentrega,mesentrega,anoentrega), _) (dia,mes,ano) | ano > anoentrega = True
                                                                          | mes > mesentrega && ano == anoentrega = True
                                                                          | dia > diaentrega && mes == mesentrega && ano == anoentrega = True
                                                                          | otherwise = False

--d)
passo2:: Int->(Int,Int) -> Int
passo2 0 (x,y) = x
passo2 n (x,y)= passo (n-1) (y, x+y)

fibo2:: Int -> Int
fibo2 n = let x = n  in passo x (0,1) 

--e)
prodintervalo2::Int->Int->Int
prodintervalo2 m n = if(n < m) then 1
                    else m * prodintervalo (m+1) n

fatorial2::Int->Int
fatorial2 n = let x =1; y=n in prodintervalo x y
--3 OK

--1)
 --(\x. 2*x +1) 3 = 7
--2)
 --(\ xy. x-y) 5 7 = -2
--3)
 --(\ yx. x-y) 5 7 = 2
--4)
 --(\xy. x-y)(\z. z/2) = (\xy. x-y)(\z. z/2) 
--5)
 --(\xy. x-y) ((\z. z/2)6)1 = (\xy. x-y) (3) 1 = 2
--6)
 --(\x . \y . -x y) 9 4 = 5
--7)
 --(\x .xx) (\y. y) = yy

--4 OK

--Prelude> (\x -> x + 3) 5
--8
--Prelude> (\x -> \y -> x * y + 5) 3 4
--17
--Prelude> (\(x,y) -> x * y^2) (3,4)
--48
--Prelude> (\(x,y,_) -> x * y^2) (3,4,2)
--48
--Prelude> (\xs -> zip xs [1,2,3]) [4,5,6]
--[(4,1),(5,2),(6,3)]

--5 OK
a = (\x -> \y-> y)((\z-> z)(\z-> z))(\w-> w) 5
b = ((\f-> (\x-> f(f x))) (\y-> (y * y))) 3
c = ((\f-> (\x-> f(f x)))(\y->( y + y))) 5
d = ((\x-> (\y-> x + y) 5) ((\y-> y -  3) 7))
e = (((\f-> (\x-> f(f(f x)))) (\y-> (y * y))) 2)
f = (\x-> \y->  x  +  ((\x-> x - 3) y)) 5 6