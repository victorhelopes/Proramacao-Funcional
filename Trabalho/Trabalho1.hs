-- Primeiro Trabalho de Programação Funcional
-- Feito pelos alunos:
-- Victor Hugo Eustáquio Lopes - 11721BCC011
-- Yago Vinícius Ferreira de Castro - 11721BCC020

--1)

triangulo:: Int -> Int -> Int -> String
triangulo a b c 
 | (a + b + c) == 180 && (a == b && a == c) = "Equilatero" 
 | (a + b + c) == 180 && (a == 90 || b == 90 || c == 90) = "Retangulo"
 | (a + b + c) == 180 && (a > 90 || b > 90 || c > 90) = "Obtuso"
 | (a + b + c) == 180 = "Simples"
 | otherwise = "Nao triangulo"

--2)

equacao:: Float->Float->Float->(Float,Float)
equacao a b c = if ( a == 0 ) then (((-c)/b), a)
                else ( ( ( -b + sqrt( b**2 -4*a*c) )/(2*a) ) , ( ( -b - sqrt( b**2 -4*a*c) )/(2*a) ) )

--3)

type Data = (Int, Int, Int)

precoPassagem:: Float -> Data -> Data -> Float
precoPassagem x (a,b,c) (d,e,f)
 | (f - c < 2) || (f - c == 2 && e < b) || (f - c == 2 && e == b && d < a) = (x * 15) / 100
 | (f - c <= 10) || (f - c == 11 && e < b) || (f - c == 11 && e == b && d < a) = (x * 40) / 100
 | (f - c > 70) || (f - c == 70 && e > b) || (f - c == 70 && e == b && d >= a) = (x * 50) / 100 
 | otherwise = x 

--4)

--a
gera1::[Int]
gera1 = [ (x*x) | x <- [4,5..14], mod x 2 == 0 ]

--b
gera2::[(Int,[Int])]
gera2 = [(x,[x,x+1..2*x]) | x <-[1,2..4] ]

--c
gera3:: [Int]->[Int]
gera3 (x:xs) = [ r | y<-(x:xs), r <- [1..y]]

--d
gera4:: [(Int,Int)]
gera4 = [(x,x+1) | x <- [1,3 .. 15]]

--e
gera5:: [Int]
gera5 = [ (a+b) | (a,b)<-gera4 ]

--5)

--a

contaNegM2:: [Int] -> Int
contaNegM2 x = length [ i| i <- x, i < 0 && i `mod` 2 == 0]

--b

listaNegM2:: [Int] -> [Int]
listaNegM2 x = [ i| i <- x, i < 0 && i `mod` 2 == 0]

--6)

distancia::[(Float,Float)]->[Float]
distancia xs = [ sqrt(x**2 + y**2)|(x,y)<-xs ]

--7)

fatores:: Int -> [Int]
fatores x = [i | i <- [1..x], x `mod` i == 0]

primos:: Int -> Int -> [Int]
primos x y = [i | i <- [x..y], fatores i == [1, i]]

--8)

mdc:: Int->Int->Int
mdc a b | a < b = mdc b a
        | b == 0 = a
        |otherwise = mdc b (mod a b)

mmc2::Int->Int->Int
mmc2 a b = div (a*b) (mdc a b)

mmc::Int->Int->Int->Int
mmc a b c = mmc2 a (mmc2 b c)

--9)

nIterations:: Float -> Int -> Float
nIterations x n
 | n == 1 = (fromIntegral n) / x
 | n `mod` 2 == 0 = (x / (fromIntegral n)) + nIterations x (n-1)
 | otherwise = ((fromIntegral n) / x) + nIterations x (n-1)
 
--10)

fizzbuzz::Int->[String]
fizzbuzz 1 = ["No"]
fizzbuzz n = if (mod n 3 == 0 && (mod  n 5) == 0) then fizzbuzz (n-1) ++ ["Fizzbuzz"] 
             else if (mod n 3 == 0) then fizzbuzz (n-1) ++ ["Fizz"] 
                  else if (mod n 5 == 0) then fizzbuzz (n-1) ++ ["Buzz"]
                       else fizzbuzz (n-1) ++ ["No"]

--11)

conta_ocorrencias:: Int -> Int -> [Int] -> (Int,Int)
conta_ocorrencias a b lst = ((length [i | i <- lst, i == a]), (length [j | j <- lst, j == b]))

--12)

ocorrencia::Int->[Int]->[Int]
ocorrencia _ [] = []
ocorrencia n (x:xs) = if(n == x) then x: ocorrencia n xs
                      else ocorrencia n xs
unica_ocorrencia::Int->[Int]->Bool
unica_ocorrencia n xs | ocorrencia n xs == [n] = True
                      | otherwise = False

--13)

intercala:: [a] -> [a] -> [a]
intercala x [] = x
intercala [] y = y
intercala (x:z) (y:w) = x:y : intercala z w

--14)

type Nome = String
type Endereco = String
type Telefone = Int
type Email = String

type Contato = (Nome,Endereco,Telefone,Email)
type Agenda = [Contato]

cont1::Contato
cont1 = ("cont1","rua joaquim",12345678,"emailcont1")

cont2::Contato
cont2 = ("cont2","rua joaquim",12345678,"emailcont2")

agenda::Agenda
agenda = [cont1,cont2]

recupera:: String->Agenda->String
recupera _ [] = "email desconhecido"
recupera email_rec ((nome,endereco,telefone,email):cont_rest) = if (email_rec == email) then nome
                                                else recupera email cont_rest

recupera_nome:: String->String
recupera_nome email = recupera email agenda

--15)

type Pessoa = (String, Float, Int, Char)

pessoas:: [Pessoa]
pessoas = [("Rosa", 1.66, 27,'F'),
           ("João", 1.85, 26, 'C'),
           ("Maria", 1.55, 62, 'S'),
           ("Jose", 1.78, 42, 'C'),
           ("Paulo", 1.93, 25, 'S'),
           ("Clara", 1.70, 33, 'C'),
           ("Bob", 1.45, 21, 'C'),
           ("Rosana", 1.58, 39, 'S'),
           ("Daniel", 1.74, 72, 'S'),
           ("Jocileide", 1.69, 18, 'S')]

--a

alturaMedia:: [Pessoa] -> Float
alturaMedia pessoas = sum [x | (_,x,_,_) <- pessoas] / (fromIntegral (length pessoas))

--b

maisNova:: [Pessoa] -> Int
maisNova pessoas = minimum [x | (_,_,x,_) <- pessoas]

--c

nomecVelha:: [Pessoa] -> [(String,Char)]
nomecVelha pessoas = [(nome,eCivil) | (nome,_,x,eCivil) <- pessoas, x == (maximum [y | (_,_,y,_) <- pessoas])]  

nomecVelha2:: [Pessoa] -> (String, Char)
nomecVelha2 ((nome,_,idade,eCivil):y) 
 | idade == (maximum [i | (_,_,i,_) <- pessoas]) = (nome,eCivil)
 | otherwise = nomecVelha2 y

--d

allData50:: [Pessoa] -> [Pessoa]
allData50 pessoas = [(nome,altura,idade,eCivil) | (nome,altura,idade,eCivil) <- pessoas, idade >= 50]  

--e

allMarriedI:: [Pessoa] -> Int -> Int
allMarriedI pessoas i = length [nome | (nome,_,idade,eCivil) <- pessoas, idade > i && eCivil == 'C']

--16)

insere_ord::(Ord t)=>t->[t]->[t]
insere_ord n [] = [n]
insere_ord n (x:xs)| (n < x) = n:(x:xs)
                 | otherwise = x : insere_ord n xs

--17)

reverte:: [a] -> [a]
reverte [x] = [x]
reverte (x:z) = (reverte z) ++ [x]  

--18)

n_repetidos::(Eq t)=>[t]->[t]->[t]
n_repetidos [] a = a
n_repetidos (a:b) x = if(verifica a x) then n_repetidos b (a:x)
                          else n_repetidos b (x)

verifica::(Eq t)=>t->[t]->Bool
verifica a [] = True
verifica a (x:y) |(a==x) = False
                 |otherwise = verifica a y

sem_repetidos::(Eq t)=>[t]->[t]
sem_repetidos (x:xs) = reverse(n_repetidos (x:xs) [])

--19)

disponivel:: [Int]
disponivel = [1,2,5,10,20,50,100]

notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco n = [x:y | x <- disponivel, n >= x, y <- notasTroco (n-x) ]  

--20)

nRainhas::Int->[[Int]]
nRainhas n = percorre_possibilidades(todas_Pos (cria_lista n))

cria_lista n = [1..n]

removeElem a [] = []
removeElem a (x:xs) = if(a == x) then xs else x: removeElem a xs

todas_Pos:: [Int]->[[Int]]
todas_Pos [] = [[]]
todas_Pos  l =  [ x:y | x<-l, y<-todas_Pos(removeElem x l) ] 

percorre_possibilidades [] = []
percorre_possibilidades (x:xs) = if (verifica_ataque x) then x: percorre_possibilidades xs else percorre_possibilidades xs 

verifica_ataque [] = True
verifica_ataque (x:xs) = if(ataque 1 x xs == False) then verifica_ataque xs else False 

ataque:: Int->Int->[Int]->Bool
ataque _ a [] = False
ataque n x (f:xs) = if( (x == f) || (x+n == f) || (x-n == f) ) then True -- msm linha | diagonal pra baixo | diagonal pra cima
                  else ataque (n+1) x xs



