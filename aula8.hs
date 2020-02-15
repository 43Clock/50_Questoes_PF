

data Cores = Vermelho Int
           | Verde Int
           | Azul Int

e :: Cores
e = Vermelho 20
v = Verde 80
a = Azul 50

lc :: [Cores]
lc = [e,v,a]

coresIguais :: Cores -> Cores -> Bool
coresIguais (Verde _) (Verde _) = True
coresIguais (Vermelho _) (Vermelho _) = True
coresIguais (Azul _) (Azul _) = True
coresIguais _ _ = False

instance Eq Cores where
    (==) = coresIguais

intencidade :: Cores -> Int
intencidade (Verde i) = i
intencidade (Vermelho i) = i
intencidade (Azul i) = i

comparaCores :: Cores -> Cores -> Bool 
comparaCores c1 c2 = intencidade c1 > intencidade c2

instance Ord Cores where
    (<=) a b = comparaCores b a 
    (>)  a b = comparaCores a b

showCores :: Cores -> String
showCores (Vermelho i) |i <= 20 = "Vermelho Claro"
                       |i > 75 = "Vermelho Escuro"
                       |otherwise = "Vermelho"
showCores (Verde i) |i <= 20 = "Verde Claro"
                    |i > 75 = "Verde Escuro"
                    |otherwise = "Verde"
showCores (Azul i) |i <= 20 = "Azul Claro"
                   |i > 75 = "Azul Escuro"
                   |otherwise = "Azul"

instance Show Cores where
    show = showCores 


-- Questao 2
data Exp a = Const a 
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

exp' :: Exp Int
exp' = ((Const 2) `Mais` (Const 3)) `Mult` (Const 5)

exp'' :: Exp Float
exp'' = Const 2.2

exp''' :: Exp Int
exp''' = Const 25

calcula :: Num a => Exp a -> a 
calcula (Const a) = a
calcula (Simetrico a) = -(calcula a)
calcula (Mais a b) = (calcula a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)
calcula (Mult a b) = (calcula a) * (calcula b)


infixa :: Show a => Exp a -> String
infixa (Const a) = (show a)
infixa (Simetrico a) = "-" ++ "(" ++ (infixa a) ++ ")"
infixa (Mais e d) = "(" ++ (infixa e) ++ " " ++ "+" ++ " " ++ (infixa d) ++ ")"
infixa (Menos e d) = "(" ++ (infixa e) ++ " " ++ "-" ++ " " ++ (infixa d) ++ ")"
infixa (Mult e d) = "(" ++ (infixa e) ++ " " ++ "*" ++ " " ++ (infixa d) ++ ")"

instance  Show a => Show (Exp a) where
  show = infixa 

igualExp :: (Num a , Eq a) => Exp a -> Exp a -> Bool
igualExp a b = calcula a == calcula b

instance (Eq a ,Num a) => Eq (Exp a) where
  (==) a b = igualExp a b

somaExp :: Num a =>  Exp a -> Exp a -> Exp a
somaExp a b = Const ((calcula a) + (calcula b))	

instance Num a => Num (Exp a) where
    (+) a b = Const (calcula (Mais a b))
    (-) a b = Const (calcula (Menos a b))
    (*) a b = Const (calcula (Mult a b))
    abs a = undefined --como fazer esta
    signum a = (calcula (Const a))
    fromInteger a = Const (fromInteger a)

--Questao 3
