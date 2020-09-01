--1)
dobro ::  Float -> Float
dobro x = 2*x

quadruplicar :: Float -> Float
quadruplicar x = dobro(dobro x)

hipotenusa :: Float -> Float -> Float
hipotenusa co ca = sqrt(co^2 + ca^2)

distancia :: Float -> Float -> Float -> Float -> Float
distancia x1 y1 x2 y2 = sqrt((x1 - x2)^2 + (y1-y2)^2)

--2)
--Print

--3)
conversão :: Float -> (Float,Float,Float)
conversão x = (x,3.96*x,x*4.45)

--4)
bissexto :: Int -> Bool
bissexto x = if(mod x 4 == 0 && not(mod x 100 == 0) || (mod x 400 == 0)) then True
                else False

--5)
type Data = (Int, Int, Int)
bissexto2::Data->Bool
bissexto2 (dia, mes,ano) = bissexto ano

--6)
valida::Data -> Bool
valida (dia,mes,ano) = if(mes > 12 || dia > 31) then False
                        else if ((mes == 4 || mes == 6 || mes == 9 || mes == 11) && dia == 31) then False
                                else if(mes == 2 && dia>28 && not (bissexto ano)) then False
                                        else True 
--7)
precede::Data-> Data-> Bool
precede (dia1,mes1,ano1) (dia2,mes2,ano2) = if(not (valida (dia1,mes1,ano1) && valida(dia2,mes2,ano2))) then False
                                            else if(ano1 < ano2) then True
                                                else if(mes1 < mes2) then True
                                                        else if(dia1 < dia2) then True
                                                            else False
--8)
type Livro = (String, String,String,String,Int)
type Aluno = (String,String,String,Int)
type Emprestimo = (String,String,Data, Data,String)
--9)
verifica:: Data->Emprestimo ->String
verifica datahj (cod_livro,cod_aluno,data_emprestimo,data_devolucao,situacao) =  if(precede datahj data_devolucao) then "em dia"
                                                                                 else "atrasado"