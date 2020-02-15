-- Questao 1 

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

t :: BTree Int
t = Node 5 (Node 3 (Node 2 Empty Empty)
                   (Node 7 Empty Empty))
           (Node 10 (Node 4 Empty Empty)
                     Empty)
-- (a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node v e d) = 1 + max ae ad
                    where ae = altura e
                          ad = altura d

-- (b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node v e d) = 1 + contaNodos e + contaNodos d

--(c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node v Empty Empty) = 1
folhas (Node v e d) = folhas e + folhas d

--(d)
prune :: Int -> BTree a -> BTree a 
prune 0 _ = Empty
prune n Empty = Empty
prune n (Node v e d) = Node v (prune (n-1) e) (prune (n-1) d)

--(e)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node v e d) = [v]
path (h:t) (Node v e d) = if h == True then v:path t d
                                       else v:path t e

--(f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node v e d) = let dm = mirror d
                          de = mirror e
                      in (Node v dm de)
--(g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node a ea da) (Node b eb db) = Node (f a b) (zipWithBT f ea eb) (zipWithBT f da db)

--(h)
{-unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = let ea = unzipBT e
                                 da = unzipBT d
                             in ((Node a ea da),(Node b ea da),(Node c ea da))
-}
--Questao 2
-- (a)
minimo :: Ord a => BTree a -> a
minimo (Node a Empty _) = a
minimo (Node a e d) = minimo e

-- (b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty d) = d
semMinimo (Node a e d) = Node a (semMinimo e) d

--(c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin a = let x = minimo a
                y = semMinimo a
            in (x,y)

--(d)


--Questao 3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving (Show,Eq)
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno -- arvore binaria de procura


a1 :: Aluno
a1 = (10000,"Ana",ORD,Aprov 16)
a2 = (10010,"Rui",TE,Aprov 16)
a3 = (10020,"Tó",ORD,Aprov 16)
a4 = (10030,"Zé",TE,Aprov 16)
a5 = (10040,"Pedro",ORD,Faltou)

turma :: Turma
turma = Node a4 (Node a1 Empty 
                        (Node a3 (Node a2 Empty Empty) 
                                 Empty))
                (Node a5 Empty Empty)

--(a)
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node (na,no,r,nt) e d ) |n==na = True
                                   |n<na = inscNum n e
                                   |n>na = inscNum n d

--(b)
inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (na,no,r,nt) e d ) |n==no = True
                                    |otherwise = inscNome n e || inscNome n d

--(c)
traEst :: Turma -> [(Numero,Nome)]
traEst Empty = []
traEst (Node (na,no,r,nt) e d) |r == TE = traEst e ++[(na,no)]++ traEst d
                               |otherwise = traEst e ++ traEst d

--(d)
nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota n (Node (na,_,_,nt) e d) |n==na =Just nt
                              |n<na = nota n e
                              |n> na = nota n d

--(e)
percFaltas :: Turma -> Float
percFaltas a = ((contaFaltas a)/(contaTotal a))*100

contaTotal :: Turma -> Float
contaTotal Empty = 0
contaTotal (Node a e d) = 1 + contaTotal e + contaTotal d

contaFaltas :: Turma -> Float
contaFaltas Empty = 0
contaFaltas (Node (_,_,_,Faltou) e d) = 1 + contaFaltas e + contaFaltas d
contaFaltas (Node a e d) = contaFaltas e + contaFaltas d

--(f)
mediaAprov :: Turma -> Float
mediaAprov a = (somaNotas a)/(contaAprov a)

somaNotas :: Turma -> Float
somaNotas a = fromIntegral (foldr (+) 0 (aux a))
             where aux Empty = []
                   aux (Node (_,_,_,Aprov n) e d) = [n]++(aux e)++(aux d)
                   aux (Node a e d) = (aux e)++(aux d) 

contaAprov :: Turma -> Float
contaAprov Empty = 0
contaAprov (Node (_,_,_,Aprov n) e d) = 1 + contaAprov e + contaAprov d
contaAprov (Node a e d) = contaAprov e + contaAprov d

