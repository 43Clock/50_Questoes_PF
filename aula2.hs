import Data.Char

--1 a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 +(funA ys)
{--Pega no primeiro item da lista e mete-o ao quadrado. 
Depois pega na tail da primeira lista e aplica-lhe a funçao funA novamente,
pegando no primeiro elemento dessa lista e metendo-o ao quadrado 
e assim sucessivamente.Neste caso fica:
 funA [2,3,5,1]
     =[2:[3,5,1]]
 funA [2:[3,5,1]]
=2^2 + funA [3,5,1]
=4+ funA [3:[5,1]]
=4 + 3^2 + funA[5,1]=...=39--}

--1 b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0 then h:(funB t)
                               else (funB t)
{--Pega no primeiro elemento da lista (o elemento h) e verifica se é par
 funB [8,5,12]
=funB [8:[5,12]]
= 8: funB[5,12]   (pois 8 é par)
= 8: funB[5:[12]] (pois 5 é impar)
= 8: funB[12] 
=8:(12):funB[] = [8,12] --}

--1 c)

funC (x:y:t) = funC t
funC [x] = []
funC [] =[]
{-- Da-se uma lista e devolve uma lista vazia (fazer em casa)--}

funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t

--Exercicio 2
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h :dobros t 

numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (h:t) = if c==h then 1+ numOcorre c (t)
                            else numOcorre c (t)

positivos :: [Int] -> Bool
positivos [x] = x > 0 -- é o mesmo que fazer if ...then...else 
positivos (h:t) = h>0 && positivos t

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h>0 then h:soPos t
                     else soPos t

somaNeg :: [Int] -> Int
somaNeg [x] = if x<0 then x else 0
somaNeg (h:t) = if h<0 then h + somaNeg t
                       else somaNeg t 

{--tresUlt :: [a] -> [a]
   tresUlt [] = []
   tresUlt (h:t) = if length (h:t) <= 3 then (h:t)
                                     else tresUlt (tail (h:t))
                                     funciona mas tem de ser de outra forma
                                     --}
tresUlt :: [a] -> [a]
tresUlt []= []
tresUlt [x]= [x]
tresUlt (x:y:[])= [x,y]
tresUlt (x:y:z:[]) = [x,y,z]
tresUlt (x:y:z:t) = tresUlt (y:z:t)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((_,b):t) = b:segundos t

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((b,c):t) = if a == b then True else nosPrimeiros a t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):(x,y,z):t) = sumTriplos ((a+x,b+y,c+z):t) 

--Questao 3
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if isDigit h then h:soDigitos t
                               else soDigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if isLower h then 1 +minusculas t
                                else minusculas t

nums :: String -> [Int]
nums [] = []
nums (h:t) = if isDigit h then (digitToInt h):nums t
                          else nums t

-- Questao 4
type Polinomio = [Monomio]
type Monomio = (Float,Int)
