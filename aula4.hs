
import Data.Char
--Questao #2
--(a)

--Questao #3
digitAlpha :: String -> (String, String)
digitAlpha [] = ([],[])
digitAlpha l@(h:t) = let a = [x | x <- l,isAlpha x]
                         b = [x | x <- l,isDigit x]
                     in (a,b)

--Questao #4
nzp :: [Int] -> (Int,Int,Int)
nzp l = let a = n l 0
            b = z l 0
            c = p l 0
        in (a,b,c)

n :: [Int] -> Int -> Int
n [] a = a
n (h:t) a |h<0 = n t (a+1)
          |otherwise = n t a

z :: [Int] -> Int -> Int
z [] a = a
z (h:t) a |h==0 = z t (a+1)
          |otherwise = z t a

p :: [Int] -> Int -> Int
p [] a = a
p (h:t) a |h>0 = p t (a+1)
          |otherwise = p t a

-- Questao 5
mydivMod :: Integral a => a -> a -> (a,a)
mydivMod a b = let x = divd a b 0
                   y = a-(x*b)
               in (x,y)
              where divd a b n |a>b = divd (a-b) b n+1
                               |a == b = n+1
                               |otherwise = n

-- Questao 6 
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(aux t 0) + fromDigits t
                 where aux [] n = n
                       aux (h:t) n = aux t (n+1)

{-- Questao 7
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l] -}

--Questao 8
fib n = aux (0,1) n
      where aux (a,b) 0 = a
            aux (a,b) 1 = b
            aux (a,b) n = aux (b,b+a) (n-1)