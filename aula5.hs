--Questao #1
import Data.List

myany :: (a-> Bool) -> [a] -> Bool
myany _ [] = False
myany p (x:xs) = p x || myany p xs

myzipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith p (x:xs) (y:ys) = (p x y) : (myzipWith p xs ys)
myzipWith _ _ _ = []

mytakeWhile :: (a->Bool) -> [a] -> [a]
mytakeWhile p [] = []
mytakeWhile p (x:xs) |p x = x:mytakeWhile p xs
                     |otherwise = []

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile p [] = []
mydropWhile p (x:xs) |p x = mytakeWhile p xs
                     |otherwise = (x:xs)

myspan :: (a -> Bool) -> [a] -> ([a],[a]) -- Tentar perceber melhor
myspan p [] = ([],[])
myspan p (x:xs) = if p x then (x:lt,ld) else ([],x:xs)
                where (lt,ld) = myspan p xs

mydeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
mydeleteBy p a [] = []
mydeleteBy p a (h:t) = if p a h then t
                                else h: mydeleteBy p a t 

mysortOn :: Ord b => (a->b) -> [a] -> [a]
mysortOn p [] = []
mysortOn p (h:t) = aux p h (mysortOn p t) 
                 where aux p h [] = [h]
                       aux p h (x:xs) = if p h < p x then (h:x:xs)
                                                     else x:aux p h xs
--Questao #2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

l :: Polinomio
l = [(2,3), (3,4), (5,3), (4,5), (3,3), (4,4)]

{-selgrau :: Int -> Polinomio -> Polinomio
 selgrau a [] = []
 selgrau a ((h,t):r) |a == t = (h,t) : selgrau a r
                     |otherwise = selgrau a r      -}
--(a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau a p = filter f p
            where f (c,g) = g == a 

--(b)
conta :: Int -> Polinomio -> Int
conta n ((c,g):t) = foldr f 0 ((c,g):t)
                  where f (c,g) r = if g==n
                                    then r+1
                                    else r

--(c)
grau :: Polinomio -> Int
grau p = foldr f 0 p
       where f (c,g) r = max g r

grau' :: Polinomio -> Int
grau' p = foldr (\ (c,g) r -> max g r) 0 p

--(d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv l = map (derivMonomio) l

derivMonomio :: Monomio -> Monomio
derivMonomio (c,g) = (c*(fromIntegral g),g-1) --fromIntegral passa de Int para FLoat para que a multiplicaco seja possivel

--(e)
calcula :: Float -> Polinomio -> Float
calcula x p = foldr f 0.0 p
            where f (c,g) r = r + (c*(x^g))

--(f)
simp :: Polinomio -> Polinomio
simp [] = []
simp p = filter f p
       where f (c,g) = c/= 0 
 
--(g) 
mult :: Monomio -> Polinomio -> Polinomio
mult p l = simp (map (multiplicacao p ) l)

multiplicacao :: Monomio -> Monomio -> Monomio
multiplicacao (c1,g1) (c2,g2) = (c1*c2,g1+g2)

--(h)
ordena :: Polinomio -> Polinomio
ordena p = foldr f [] p
         where  f (c,g) []= [(c,g)]
                f (c,g) ((a,b):t) |(g == b && c>a) || g >b = (c,g):(a,b):t
                                  |otherwise = (a,b): f (c,g) t

--(i)
normaliza :: Polinomio -> Polinomio
normaliza p = ordena (foldr f [] p)
            where f (c,g) []= [(c,g)]
                  f (c,g) ((a,b):t) |g == b = (a+c,b):t
                                    |otherwise = (a,b): f (c,g) t 

--(j) 
soma :: Polinomio -> Polinomio -> Polinomio
soma a b = somaNormaliza (normaliza a) (normaliza b)

somaNormaliza :: Polinomio -> Polinomio -> Polinomio
somaNormaliza a b = aux a b
                  where aux a [] = a
                        aux [] a = a
                        aux a@((c,g):t) ((c',g'):r) | g' == g = (c+c',g): aux t r
                                                    |otherwise = (c,g):aux a t




--Questao 3 
type Mat a = [[a]]

m :: Num a => Mat a
m = [[1,2,3],[0,4,5],[0,0,6]]

--(a)
dimOk :: Mat a -> Bool
dimOk [] = True
dimOk [h] = True
dimOk (h:i:t) = if length h == length i then dimOk (i:t)
                                        else False

--(b)
dimMat :: Mat a -> (Int,Int)
dimMat l@(h:t) = (length l,length h)

--(c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat a@(h:t) b@(x:y) |dimMat a == dimMat b = (somar h x):addMat t y
                       |otherwise = error  "Matrizes tem dimensoes diferentes"
addMat _ _ = []

somar :: Num a =>[a] -> [a] -> [a]
somar (h:t) (x:y) = (h+x):somar t y
somar _ _ = []

--(d)
mytranspose :: Mat a -> Mat a
mytranspose (h:t) = junta (matrix h) (transpose t)

matrix :: [a] -> [[a]]
matrix t = map lista t
         where  lista a = [a]

junta :: Mat a -> Mat a -> Mat a 
junta (h:t) (x:y) = [(h++x)]++junta t y
junta _ _ = []

--(e)
--multMat :: Num a => Mat a -> Mat a -> Mat a
--multMat a@(h:t) b |colunas a == linhas b = junta [mults a (heads b)] (multMat a (heads $ unhead b))
  --                |otherwise = error "Nao é possivel multipicar"


mults :: Num a => Mat a -> [a] -> [a] 
mults [] r = []
mults (h:t) r = [(aux h r)] ++ mults t r
               where aux (h:t) (x:xs) = h*x + aux t xs 
                     aux _ _ = 0

heads :: Num a => Mat a -> [a]
heads [] = []
heads (h:t) = (head h): heads t

unhead :: Mat a -> Mat a
unhead a = map tail a

linhas :: Mat a -> Int
linhas a = fst (dimMat a)

colunas :: Mat a -> Int
colunas a = snd (dimMat a)

--(h)
rotateLeft :: Mat a -> Mat a 
rotateLeft a = reverse (mytranspose a )