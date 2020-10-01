lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1

--1
paridade::[Int]->[Bool]
paridade l = map (even) l

--2
prefixos::[String]->[String]
prefixos l = map (primeiros3) l

primeiros3 :: String -> [Char]
primeiros3 [] = []
primeiros3 (a) = a
primeiros3 (a:b:[]) = [a] ++[b]
primeiros3 (a:b:c:d) = [a]++[b]++[c]
--3
saudacao:: [String]->[String]
saudacao l = map ("Oi " ++ ) l

--4
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f [] = []
filtrar f (x:xs) = if (f x == True) then x : filtrar f xs else filtrar f xs

--5
pares::[Int]->[Int]
pares l = filter (even) l 

--6
solucoes:: [Integer]->[Integer]
solucoes l = filter (\x-> (5*x+6) <(x*x)) l

--7
maior :: (Foldable t, Ord a) => t a -> a
maior l = foldr1 (\aux -> \x -> if x > aux then x else aux)  l

--8
menor_min10 :: (Ord a, Num a) => [a] -> a
menor_min10 (x:xs) = foldr (\aux -> \x -> if  x < aux then if x>10 then 10 else x else aux) x xs

--9
junta_silabas_plural :: [[Char]] -> [Char]
junta_silabas_plural l = (foldr (++) [] l )  ++ ['s']

--10
selection :: [Int] -> [Int]
selection [] = []
selection x = let 
                [a] = [menor x]
                b = selection (remove_elem a x)
               in foldl (++) [a] [b]

menor :: [Int] -> Int
menor [x] = x
menor (x:y:xs) | x < y = menor (x:xs)
               | otherwise = menor (y:xs)

remove_elem:: Int->[Int]->[Int]
remove_elem x (a:b) | a == x = b
                    | otherwise = a : remove_elem x b

inserction :: [Int]->[Int]
inserction (a:b) = aux (a:b) []

insere :: Int -> [Int] -> [Int] 
insere x [] = [x]
insere x (a:b) | x < a = foldr (++) [x] [a:b]
               | otherwise = foldr (++) [a] [insere x b]

aux:: [Int]->[Int]->[Int]
aux [] r = r
aux (x:xs) r = aux xs (insere x r) 

quicksort::[Int]->[Int]
quicksort [] = []
quicksort (x:xs) = (quicksort esqueda) ++ [x] ++ (quicksort direita)
                  where esqueda  = filter (<x) xs
                        direita = filter (>=x) xs
trocar :: Ord a => [a] -> [a]
trocar [x] = [x]
trocar (x:y:zs)
 | x > y = y : trocar (x:zs)
 | otherwise = x : trocar  (y:zs)

bubblesort :: (Num t, Ord a, Eq t) => [a] -> t -> [a]
bubblesort lista 0 = lista
bubblesort lista n = bubblesort (trocar lista) (n-1)

bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble lista = bubblesort lista (length lista)

--11
selection2 :: [Int] -> (Int,[Int])
selection2 l= let (a,b,c) = aux1 l in (a,b)

aux1 :: [Int] -> (Int, [Int], [a])
aux1 [] = (0,[],[])
aux1 (x:xs) = let 
              (a,b,c) = remove_menor2 (x:xs) (0, x, [])
              (d,e,f) = aux1 c
             in ((a+d), b:e,f)

remove_menor2::[Int]->(Int,Int,[Int])->(Int,Int,[Int])
remove_menor2 [] (n,x,l) = (n,x,l)
remove_menor2 (a:b) (n,x,l) | x < a = remove_menor2 b (n+1,x, [a] ++ l )
                            | x == a = remove_menor2 b (n,x , l)
                            | otherwise = remove_menor2 b (n+1,a, [x] ++ l)

--
inserctionsort2 :: [Integer] -> ([Integer],Integer)
inserctionsort2 [] = ([],0)
inserctionsort2 (x: xs) = (l,c + c1)
 where
  (l1,c) = inserctionsort2 xs
  (l,c1) = insertCont x (l1,0)

insertCont :: Integer -> ([Integer],Integer) -> ([Integer],Integer)
insertCont x ([],c) = ([x],c)
insertCont x ((y:ys),c)
      | x <= y = (x : y : ys, c+1)
      | otherwise = (y :l, c1+1)
      where (l,c1) = insertCont x (ys,c)
--
bubblesort2 :: [Integer] -> ([Integer], Int)
bubblesort2 [] = ([], 0)
bubblesort2 xs = bubbleOrdContador xs (length xs)

bubbleOrdContador :: [Integer]->Int->([Integer], Int)
bubbleOrdContador xs 0 = (xs, 0)
bubbleOrdContador xs n = (d, a + b)
 where
 (aux, a) = trocaContador xs
 (d, b) = bubbleOrdContador aux (n-1)

trocaContador :: [Integer] -> ([Integer], Int)
trocaContador [x] = ([x],0)
trocaContador (x:y:z) 
 |x > y = (y:d, a + 1)
 |otherwise = (x:n, b + 1)
 
 where
 (d, a) = trocaContador(x:z)
 (n, b) = trocaContador(y:z)

--
quickSort2 :: [Integer] -> ([Integer],Integer)
quickSort2 [] = ([],0)
quickSort2 (x:xs) = (xs1 ++ [x] ++ xs2, c + c1 + c2)
 where 
    (xs3,xs4,c) = divideContador x (xs,0)
    (xs1,c1) = quickSort2 xs3
    (xs2,c2) = quickSort2 xs4


divideContador :: Integer -> ([Integer],Integer) -> ([Integer],[Integer],Integer)
divideContador x ([],c) = ([],[],0)
divideContador x ((y:ys),c)
 | y < x = (y:x1,x2,c+1)
 |otherwise = (x1,y:x2,c+1)
 where
  (x1,x2,c) = divideContador x (ys,c)


--12
selectionsort3 :: [Integer] -> (Integer,[Integer])
selectionsort3 l= let (a,b,c) = aux2 l in (a,b)

aux2 :: [Integer] -> (Integer, [Integer], [a])
aux2 [] = (0,[],[])
aux2 (x:xs) = let 
              (a,b,c) = remove_menor3 (x:xs) (0, x, [])
              (d,e,f) = aux2 c
             in ((a+d), b:e,f)

remove_menor3::[Integer]->(Integer,Integer,[Integer])->(Integer,Integer,[Integer])
remove_menor3 [] (n,x,l) = (n,x,l)
remove_menor3 (a:b) (n,x,l) | x > a = remove_menor3 b (n+1,x, l ++ [a])
                            | x == a = remove_menor3 b (n,x , l)
                            | otherwise = remove_menor3 b (n+1,a, [x] ++ l)

--
insertionSort3 :: [Integer] -> ([Integer],Integer)
insertionSort3 [] = ([],0)
insertionSort3 (x: xs) = (l,c + c1)
 where
  (l1,c) = insertionSort3 xs
  (l,c1) = insertCont2 x (l1,0)

insertCont2 :: Integer -> ([Integer],Integer) -> ([Integer],Integer)
insertCont2 x ([],c) = ([x],c)
insertCont2 x ((y:ys),c)
      | x >= y = (x : y : ys, c+1)
      | otherwise = (y : l, c1+1)
      where (l,c1) = insertCont2 x (ys,c)
--
bubbleSort3 :: [Integer] -> ([Integer], Int)
bubbleSort3 [] = ([], 0)
bubbleSort3 xs = bubbleOrdContador2 xs (length xs)

bubbleOrdContador2 :: [Integer]->Int->([Integer], Int)
bubbleOrdContador2 xs 0 = (xs, 0)
bubbleOrdContador2 xs n = (d, a + b)
 where
 (aux, a) = trocaContador2 xs
 (d, b) = bubbleOrdContador2 aux (n-1)

trocaContador2 :: [Integer] -> ([Integer], Int)
trocaContador2 [x] = ([x],0)
trocaContador2 (x:y:z) 
 |x > y = (x:d, a + 1)
 |otherwise = (y:n, b + 1)
 
 where
 (d, a) = trocaContador2(y:z)
 (n, b) = trocaContador2(x:z)

--
quickSort3 :: [Integer] -> ([Integer],Integer)
quickSort3 [] = ([],0)
quickSort3 (x:xs) = (xs1 ++ [x] ++ xs2, c + c1 + c2)
 where 
    (xs3,xs4,c) = divideContador2 x (xs,0)
    (xs1,c1) = quickSort3 xs3
    (xs2,c2) = quickSort3 xs4


divideContador2 :: Integer -> ([Integer],Integer) -> ([Integer],[Integer],Integer)
divideContador2 x ([],c) = ([],[],0)
divideContador2 x ((y:ys),c)
 | y < x = (x1,y:x2,c+1)
 |otherwise = (y:x1,x2,c+1)
 where
  (x1,x2,c) = divideContador2 x (ys,c)

{-
11) 
selection lst1 =  190
selection lst2 =  190
selection lst3 =  190
selection lst4 =  190
selection lst5 =  190
selection lst6 =  190
selection lst7 =  499500
selection lst8 =  499500
selection lst9 =  210
selection lst10 = 210
selection lst11 = 420
selection lst12 = 420 


inserctionsort lst1 =  19
inserctionsort lst2 =  190
inserctionsort lst3 =  118
inserctionsort lst4 =  100
inserctionsort lst5 =  109
inserctionsort lst6 =  83
inserctionsort lst7 =  999
inserctionsort lst8 =  499500
inserctionsort lst9 =  39
inserctionsort lst10 =  119
inserctionsort lst11 =  349
inserctionsort lst12 = 350


bubblesort2 lst1 =  380
bubblesort2 lst2 =  380
bubblesort2 lst3 =  380
bubblesort2 lst4 =  380
bubblesort2 lst5 =  380
bubblesort2 lst6 =  380
bubblesort2 lst7 =  999000
bubblesort2 lst8 =  999000
bubblesort2 lst9 =  420
bubblesort2 lst10 =  420
bubblesort2 lst11 =  1640
bubblesort2 lst12 = 1640

quickSort2 lst1 =  190
quickSort2 lst2 =  190
quickSort2 lst3 =  100
quickSort2 lst4 =  100
quickSort2 lst5 =  80
quickSort2 lst6 =  84
quickSort2 lst7 =  499500
quickSort2 lst8 =  499500
quickSort2 lst9 =  191
quickSort2 lst10 =  120
quickSort2 lst11 =  420
quickSort2 lst12 = 240


12) 
selectionsort3 lst1 =  190
selectionsort3 lst2 =  190
selectionsort3 lst3 =  190
selectionsort3 lst4 =  190
selectionsort3 lst5 =  190
selectionsort3 lst6 =  190
selectionsort3 lst7 =  499500
selectionsort3 lst8 =  499500
selectionsort3 lst9 =  210
selectionsort3 lst10 = 210
selectionsort3 lst11 = 400
selectionsort3 lst12 = 4040


inserctionsort3 lst1 =  20
inserctionsort3 lst2 =  20
inserctionsort3 lst3 =  20
inserctionsort3 lst4 =  20
inserctionsort3 lst5 =  20
inserctionsort3 lst6 =  20
inserctionsort3 lst7 =  499500
inserctionsort3 lst8 =  999
inserctionsort3 lst9 =  210
inserctionsort3 lst10 = 120
inserctionsort3 lst11 = 520
inserctionsort3 lst12 = 510


bubblesort3 lst1 =  380
bubblesort3 lst2 =  380
bubblesort3 lst3 =  380
bubblesort3 lst4 =  380
bubblesort3 lst5 =  380
bubblesort3 lst6 =  380
bubblesort3 lst7 =  999000
bubblesort3 lst8 =  999000
bubblesort3 lst9 =  420
bubblesort3 lst10 =  420
bubblesort3 lst11 =  1640
bubblesort3 lst12 = 1640

quickSort2 lst1 =  190
quickSort2 lst2 =  190
quickSort2 lst3 =  100
quickSort2 lst4 =  100
quickSort2 lst5 =  80
quickSort2 lst6 =  84
quickSort2 lst7 =  499500
quickSort2 lst8 =  499500
quickSort2 lst9 =  191
quickSort2 lst10 = 120
quickSort2 lst11 = 420
quickSort2 lst12 = 240
-}