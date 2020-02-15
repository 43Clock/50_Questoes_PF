import Data.Char

-- Exercicio #1
perimetroC :: Double -> Double
perimetroC raio = 2 * pi * raio

dist1 :: (Double,Double) -> (Double,Double) -> (Double)
dist1 (x1,y1) (x2,y2) = sqrt ( (x1 - x2)^2 + (y1 - y2)^2)

primUlt :: [a] -> (a,a)
primUlt lista = (head lista , last lista)

multiplo :: (Int,Int) -> Bool
multiplo (x,y) = if (mod x y) == 0 then True else False

-- mutiplo e multiplo2 fazem a msm coisa.

multiplo2 x y = mod x y == 0

truncaImpar :: [a] -> [a]
truncaImpar lista = if (mod (length lista) 2 ==0) then lista else tail lista

max2 :: (Int,Int) -> Int
max2 (x,y) = if x >= y then x else y

max3 :: (Int,Int,Int)-> (Int)
max3 (x,y,z) = max2 (max2(x,y),z)

-- Exercicio #2
nRaizes :: (Double,Double,Double) -> Int
nRaizes (a,b,c) | b^2 - 4*a*c >= 0 = 2
                | b^2 - 4*a*c == 0 = 1
                | b^2 - 4*a*c < 0 = 0

raizes :: (Double,Double,Double) -> [Double]
raizes (a,b,c) |nRaizes (a,b,c) == 2 = [((-b + sqrt(b^2 -4*a*c))/2*a),((-b - sqrt(b^2 -4*a*c))/2*a)]
               |nRaizes (a,b,c) == 1 = [(-b)/2*a] 
               |nRaizes (a,b,c) == 0 = []

-- Exercicio #3
type Hora = (Int,Int)

valida :: Hora -> Bool
valida (h,m) |h>=0 && h<24 && m>=0 && m<60 = True 
             |otherwise = False

comparacao :: Hora -> Hora -> Bool
comparacao (h1,m1) (h2,m2) |h1 > h2 = True
                           |h1 == h2 && m1>m2 = True
                           |otherwise = False

comparacao' (h1,m1) (h2,m2) = h1>h2 ||
                            (h1 == h2 && m1>m2)

type Minutos = Int

converter :: Hora -> Minutos
converter (h1,m1) = h1*60 + m1

converterMin :: Minutos -> Hora
converterMin m1 = ((div m1 60),((mod m1 60)))

converterMin' m1 | m1>=0 && m1<= 1439 = (div m1 60,mod m1 60)

difHoras :: Hora -> Hora -> Minutos
difHoras (h1,m1) (h2,m2) = (abs(h2-h1)*60 + abs(m2-m1))

addMin :: Minutos -> Hora -> Hora
addMin m2 (h1,m1) = converterMin(converter (h1,m1) + m2)

-- Exercicio #4
data Horas = H Int Int deriving (Show,Eq)

horaValida :: Horas -> Bool
horaValida (H h m) |h>=0 && h<24 && m>=0 && m<60 = True 
                |otherwise = False

comparacaoH :: Horas -> Horas -> Bool
comparacaoH (H h1 m1) (H h2 m2) |h1>h2 = True
                                |h1==h2 && m1>m2 = True
                                |otherwise = False

converterH :: Horas -> Minutos
converterH (H h1 m1) = (h1*60) + m1

converterMinH :: Minutos -> Horas
converterMinH m1 = (H (div m1 60) (mod m1 60))

difHorasH :: Horas -> Horas -> Minutos
difHorasH (H h1 m1) (H h2 m2) = (abs(h2-h1)*60 + abs(m2-m1))

addMinH :: Minutos -> Horas -> Horas
addMinH m2 (H h1 m1) = converterMinH(converterH (H h1 m1)+m2)

-- Exercicio #5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq) --cria novo tipo chamado Semaforo

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 |stop s1 == True && stop s2 ==False = True
           |stop s1 == False && stop s2 == True = True
           |stop s1 == True && stop s2 == True = True
           |otherwise = False
           
safe':: Semaforo -> Semaforo -> Bool
safe' Vermelho _ = True
safe' _ Vermelho = True
safe' _ _ = False

-- Exercicio #6
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = abs(x)
posx (Polar d a) = abs(d*cos a)

posy :: Ponto -> Double
posy (Cartesiano x y) = abs(y)
posy (Polar d a) = abs(d*sin a)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2+y^2)
raio (Polar d a) = d

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan(y/x)
angulo (Polar d a) = a 

dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ( (x1 - x2)^2 + (y1 - y2)^2)
--dist (Polar d1 a1) (Polar d2 a2) =

-- Exercicio #7
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo p1 a) = False
poligono _ = True

vertices :: Figura -> [Ponto]
vertices (Triangulo a b c) = [a,b,c]
vertices (Rectangulo (Cartesiano x y) (Cartesiano a b)) = [(Cartesiano x y),(Cartesiano x b),(Cartesiano a b),(Cartesiano a y)]
vertices h = []

area :: Figura -> Double
area (Triangulo p1 p2 p3) = let a = dist p1 p2
                                b = dist p2 p3
                                c = dist p3 p1
                                s = (a+b+c) / 2 -- semi-perimetro
                            in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

area (Rectangulo (Cartesiano x y) (Cartesiano a b)) = abs (x-y) * abs(a-b)
area (Circulo p r) = r*r*pi

perimetro :: Figura -> Double
perimetro (Circulo p1 raio) = 2 * pi* raio
perimetro (Triangulo p1 p2 p3) = dist p1 p2 + dist p2 p3 + dist p3 p1
perimetro (Rectangulo (Cartesiano x y) (Cartesiano a b)) = 2 * abs (x-y) + 2 * abs(a-b) 


-- Exercicio 8
isLower' :: Char -> Bool
isLower' a = ord a >= ord 'a' && ord a <= ord 'z' 

isDigit' :: Char -> Bool
isDigit' c = (ord 'c' >= ord '0') && (ord 'c' <= ord '9')

isAlpha' :: Char -> Bool
isAlpha' c = ((ord 'c' >= ord 'a') && (ord 'c' <= ord 'z'))
            || ((ord 'c' >= ord 'A') && (ord 'c' <= ord 'Z'))

toUpper' :: Char -> Char
toUpper' a = chr $ (ord a)-32


