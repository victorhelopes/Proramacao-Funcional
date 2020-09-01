--Nome: Victor Hugo Eustáquio Lopes
--Matricula: 11721BCC011

--1
ouc::Bool->Bool->Bool
ouc _ True = True
ouc True _ = True
ouc False False =  False

ouif::Bool->Bool->Bool
ouif x y = if(x == True ) then True 
            else if(y == True) then True
                else False
--2
type Ponto = (Float,Float)
distancia ::Ponto->Ponto->Float
distancia (x1,y1) (x2,y2) = sqrt((x1 - x2)^2 + (y1-y2)^2) 

--3
--Print

--4
fatorialg::Int->Int
--Usando guardas
fatorialg x 
    | x == 0 = 1
    | x <0 = 0
    |otherwise = x * fatorialg (x-1)

--casamento de padroes
fatorialc::Int->Int
fatorialc 0 = 1
fatorialc x = x * fatorialc (x-1)

--5
fibo:: Int->Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-2) + fibo (n-1)

--6
n_tri::Int->Int
n_tri 0 = 0
n_tri n = (n-1) + n_tri (n-1)

--7
passo:: Int->(Int,Int) -> Int
passo 0 (x,y) = x
passo n (x,y) = passo (n-1) (y, x+y)

fibo7:: Int -> Int
fibo7 n = passo n (0,1)

--8
potencia2::Int->Int
potencia2 0 = 1
potencia2 n = 2 * potencia2 (n-1)

--9
--a
prodintervalo::Int->Int->Int
prodintervalo m n = if(n < m) then 1
                    else m * prodintervalo (m+1) n
--b
fatorial9::Int->Int
fatorial9 n = prodintervalo 1 n

--11
resto_div:: Int->Int->Int
resto_div m n | m > n = resto_div (m-n) n
              | otherwise = m

div_inteira::Int->Int->Int
div_inteira m n |m >= n = 1 + div_inteira (m-n) n
                | otherwise = 0

--12
mdc:: (Int,Int) ->Int
mdc (m,n) | n==0 = m
          | otherwise = mdc(n , (mod m n))

mdc_casamento::(Int,Int)->Int
mdc_casamento (m,0) = m 
mdc_casamento (m,n) = mdc_casamento(n,(mod m n))

--13
binomial:: (Int,Int)->Int
binomial (n,k)| k == 0 = 1
              | n == k = 1
              | otherwise = binomial((n-1),k) + binomial((n-1),(k-1))

binomial_casamento::(Int,Int)->Int
binomial_casamento (_,0) = 1
binomial_casamento (n,k) = if(n == k) then 1 
                            else binomial_casamento((n-1),k) + binomial_casamento((n-1),(k-1))

--14
--[5,4..1]
--['a','c' .. 'e']
--[1,4..16]
-- zip[1,(-2)..(-11)][1,5..17]

--15
lista::Int->Int->[Int]
lista a b | a == b = [a]
          | a > b = []
          | otherwise = [a..b]

lista_pares:: Int->Int->[Int]
lista_pares a b | a > b = []
                | a == b && even a = [a]
                | otherwise = if even a then a : lista_pares (a+1) b
                                else lista_pares (a+1) b
