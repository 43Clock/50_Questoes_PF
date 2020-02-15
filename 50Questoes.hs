--1
myenumFromTo :: Int -> Int -> [Int]
myenumFromTo a b |a>b = []
                 |a == b = [a]
                 |otherwise = a:myenumFromTo (a+1) b

--2
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo a b c |(a == b && a>=c) = repeat a
                       |a==c = [a]
                       |(a < b && b < c) = a:myenumFromThenTo b (b+(b-a)) c 
                       |otherwise = []

--3 
myplusplus :: [a] -> [a] -> [a]
myplusplus l [] = l
myplusplus [] l = l
myplusplus (h:hs) t = h:myplusplus hs t

--4
(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) n = (!!!) t (n-1)

--5
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myreverse t ++ [h]

--6
mytake :: Int -> [a] -> [a]
mytake 0 l = []
mytake n [] = []
mytake n (h:t) = h:mytake (n-1) t

--7
mydrop :: Int -> [a] -> [a]
mydrop 0 l = l
mydrop _ [] = []
mydrop n (h:t) = mydrop (n-1) t

--8
myzip :: [a] -> [b] -> [(a,b)]
myzip (h:t) (x:xs) = (h,x) : myzip t xs
myzip _ _ = []

--9
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem n (h:t) = if n == h then True
                           else myelem n t

--10
myreplicate :: Int -> a -> [a]
myreplicate 0 a = []
myreplicate n a = a:myreplicate (n-1) a

--11 
myintersperse :: a -> [a] -> [a]
myintersperse n [] = []
myintersperse n [a] = [a]
myintersperse a (h:t) = h:a:myintersperse a t

--12
mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup l = take (contaseguidos l) l : mygroup (drop (contaseguidos l) l)

contaseguidos :: Eq a => [a] -> Int
contaseguidos [a] = 1
contaseguidos (h:i:t) = if h == i then 1+contaseguidos (i:t)
                                  else 1

--13
myconcat :: [[a]] -> [a]
myconcat [[]] = []
myconcat (h:t) = h ++ myconcat t

--14
myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l = myinits (init l) ++ [l]

--15
mytails :: [a] -> [[a]]  
mytails [] = [[]]
mytails l = [l] ++ mytails (tail l)

--16
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] l = True
isPrefixOf (h:t) (x:xs) = if h == x then isPrefixOf t xs
                                    else False

--17
isSufixOf :: Eq a => [a] -> [a] -> Bool
isSufixOf [] l = True
isSufixOf h t = if last h ==last t then isSufixOf (init h) (init t)
                                   else False

--18
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] l = True
isSubsequenceOf l [] = False
isSubsequenceOf (h:t) (x:xs) |h == x = isSubsequenceOf t  xs
                             |h /= x = isSubsequenceOf (h:t) xs 

--19
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n [] = []
elemIndices n (h:t) = aux 0 n (h:t)
                    where aux a n [] = []
                          aux a n (h:t) = if n == h then a : aux (a+1) n t
                                                    else aux (a+1) n t

--20
nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) = h:nub (aux h t)
          where aux h [] = []
                aux h (i:t) |h == i = aux h t
                            |otherwise = i:aux h t

--21
mydelete :: Eq a => a -> [a] -> [a]
mydelete n [] = []
mydelete n (h:t) = if n == h then t
                             else h:mydelete n t

--22
myslash :: Eq a => [a] -> [a] -> [a]
myslash l [] = l
myslash l (h:t) = if elem h l then myslash (mydelete h l) t
                              else myslash l t

--23
union :: Eq a => [a] -> [a] -> [a]
union [] l = l
union l [] = l
union l (h:t) = if elem h l then union l t
                            else union (l++[h]) t

--24
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] l = []
intersect (h:t) l = if elem h l then h:intersect t l
                                else intersect t l

--25
myinsert :: Ord a => a -> [a] -> [a]
myinsert a [] = [a]
myinsert a (h:t) |a<=h = (a:h:t)
                 |otherwise = h:myinsert a t 

--26
myunwords :: [String] -> String
myunwords [] = ""
myunwords [h] = h
myunwords (h:t) = h ++ " " ++ myunwords t

--27
myunlines :: [String] -> String
myunlines [] = ""
--myunlines [h] = h ++ "\n"
myunlines (h:t) = h ++ "\n" ++ myunlines t

--28
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = posicao (maior h t) (h:t) 
             where maior h [] = h
                   maior h (x:xs) |h>x = maior h xs
                                  |otherwise = maior x xs
                   posicao h (x:xs) |h==x = 0
                                    |otherwise = 1+ posicao h xs

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos [a] = False
temRepetidos (h:t) = if aux h t then True
                                else temRepetidos t 
                   where aux n [] = False             --aux é a elem
                         aux n (h:t) |h == n = True
                                     |otherwise = aux n t

--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) |h>='0' && h<='9' = h:algarismos t
                 |otherwise = algarismos t

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [a] = []
posImpares (h:i:t) = i:posImpares t

--32
posPares :: [a] -> [a]
posPares [] = []
posPares [a] = [a]
posPares (h:i:t) = h:posPares t

--33 
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [h] = True
isSorted (h:i:t) |h>i = False
                 |otherwise = isSorted (i:t)

--34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = myinsert h (iSort t)

--35
menor :: String -> String ->  Bool
menor h t = if h<t then True
                   else False

--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _  [] = False
elemMSet a ((x,y):t) |x == a = True 
                     |otherwise = elemMSet a t

--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):t) = y + lengthMSet t

--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):t) = (replicate y x) ++ (converteMSet t)

--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t) = if a == x then ((x,y+1):t)
                                   else (x,y) : insereMSet a t

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((x,y):t) |a==x && y ==1 = t
                       |a==x = ((x,y-1):t)
                       |otherwise = (x,y):removeMSet a t 

--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = aux l []
               where aux [] l  = l
                     aux (h:t) l = aux t (insereMSet h l)

--42
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers l = let a = letfs l 
                         b = rigths l
                     in (a,b)

letfs :: [Either a b] -> [a]
letfs [] = []
letfs (Left a :t) = a:letfs t
letfs (Right b :t) = letfs t

rigths :: [Either a b] -> [b]
rigths [] = []
rigths (Left a :t) = rigths t
rigths (Right b :t) = b :rigths t

--43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t) = catMaybes t
catMaybes (Just a :t) = a:catMaybes t

--44
data Movimento = Norte | Sul | Este | Oeste
                deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) = case h of
    Norte -> posicao (x,y+1) t
    Sul -> posicao (x,y-1) t
    Este -> posicao (x+1,y) t
    Oeste -> posicao (x-1,y) t

--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (m,n) |x==m && y==n = []
                    |x<m = Este :caminho (x+1,y) (m,n)
                    |x>m = Oeste:caminho (x-1,y) (m,n)
                    |y<n = Norte:caminho (x,y+1) (m,n)
                    |y>n = Sul  :caminho (x,y-1) (m,n)

--46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) = case h of
    Norte -> vertical t
    Sul -> vertical t
    Este -> False
    Oeste -> False

--47
data Posicao = Pos Int Int
             deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [h] = h
maisCentral (h:i:t) = maisCentral ((centro h i):t)
                    where centro (Pos x y) (Pos m n) |(x^2 + y^2) < (m^2 + n^2) = Pos x y
                                                     |otherwise = Pos m n 

--48
vizinho :: Posicao -> [Posicao] -> [Posicao]
vizinho p [] = []
vizinho (Pos x y) ((Pos m n):t) |y==n && (m==x-1||m==x+1) = (Pos m n):vizinho (Pos x y) t
                                |x==m && (n==y-1||n==y+1) = (Pos m n):vizinho (Pos x y) t
                                |otherwise = vizinho (Pos x y) t

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [a] = True
mesmaOrdenada ((Pos x y):(Pos m n):t) = if y /= n then False
                                                  else mesmaOrdenada ((Pos m n):t)

--50
data Semaforo = Verde | Amarelo | Vermelho
              deriving Show

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK l = (naoVermelho l)< 2
              where naoVermelho [] = 0
                    naoVermelho (h:t) = case h of
                        Vermelho -> naoVermelho t
                        otherwise -> 1+naoVermelho t