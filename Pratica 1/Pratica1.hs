--Exercicio 2
dobro :: Num a => a -> a
dobro x = 2*x

--Exercicio 3
quadruplicar :: Num a => a -> a
quadruplicar x = dobro(dobro x)

--Exercicio 4
hipotenusa :: Floating a => a -> a -> a
hipotenusa co ca = sqrt(co^2 + ca^2)

--Exercicio 5
distancia :: Floating a => a -> a -> a -> a -> a
distancia x1 y1 x2 y2 = sqrt((x1 - x2)^2 + (y1-y2)^2)
