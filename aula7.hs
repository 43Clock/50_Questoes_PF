-- Questao 1
data ExpInt = Const     Int
            | Simetrico ExpInt
            | Mais      ExpInt ExpInt
            | Menos     ExpInt ExpInt
            | Mult      ExpInt ExpInt


e :: ExpInt
e = (Const 2) `Mais` ((Const 3) `Mult` (Const 5))
--(a)

calcula :: ExpInt -> Int
calcula (Const a) = a
calcula (Simetrico a) = -(calcula a)
calcula (Mais e d) = (calcula e) + (calcula d)
calcula (Menos e d) = (calcula e) - (calcula d)
calcula (Mult e d) = (calcula e) * (calcula d)

infixa :: ExpInt -> String
infixa (Const a) = (show a)
infixa (Simetrico a) = "-" ++ "(" ++ (infixa a) ++ ")"
infixa (Mais e d) = "(" ++ (infixa e) ++ " " ++ "+" ++ " " ++ (infixa d) ++ ")"
infixa (Menos e d) = "(" ++ (infixa e) ++ " " ++ "-" ++ " " ++ (infixa d) ++ ")"
infixa (Mult e d) = "(" ++ (infixa e) ++ " " ++ "*" ++ " " ++ (infixa d) ++ ")"

posfixa :: ExpInt -> String
posfixa (Const a) = (show a)
posfixa (Simetrico a) = "-(" ++ (infixa a) ++ ")"
posfixa (Mais e d) = posfixa e ++ " " ++ posfixa d ++ "+"
posfixa (Menos e d) = posfixa e ++ " " ++ posfixa d ++ "-"
posfixa (Mult e d) = posfixa e ++ " " ++ posfixa d ++ "*"


--Questao #2
data RTree a = R a [RTree a]    

rt :: RTree Int
rt = R 5 [ R 7 []
         , R 2 [ R 4 []
               , R 5 []]
         , R 4 [ R 7 []]]

soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a l) = a + sum (map soma l)

altura :: RTree a -> Int
altura (R _ []) = 1
altura (R v l) = 1+ maximum (map altura l)

prune :: Int -> RTree a -> RTree a
prune 1 (R v l) = R v []
prune n (R v l) = R v (map (prune (n-1)) l)

mirror :: RTree a -> RTree a
mirror (R v l) = reverse (map mirror l)

postorder :: RTree a -> [a]