--Exercicio #1

data Hora = H Int Int deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

v:: Viagem
v = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

horaValida :: Hora -> Bool
horaValida (H h m) |h>=0 && h<24 && m>=0 && m<60 = True 
                   |otherwise = False

testarEtapa :: Etapa -> Bool
testarEtapa (H h1 m1, H h2 m2) = horaValida (H h1 m1) && 
                                 horaValida (H h2 m2) &&
                                 (h2>h1) || (h1 ==h2 && m2> m1)

testarViagem :: Viagem -> Bool
testarViagem [e] = testarEtapa e
testarViagem (e1:e2:es) = testarEtapa e1 && 
                          testarEtapa ((snd e1),(fst e2)) && 
                          testarViagem (e2:es)

horaPartidaChegada :: Viagem -> (Hora,Hora)
horaPartidaChegada v = (fst (head v), snd (last v))

horaParaMin :: Hora -> Int
horaParaMin (H h m) =h*60 +m

minParaHora :: Int -> Hora
minParaHora m = H (div m 60) (mod m 60)

tempoEfetivo :: Viagem -> Int
tempoEfetivo [] = 0
tempoEfetivo (e:es) = duracaoEtapa e + tempoEfetivo es
        where duracaoEtapa (h1,h2) = (horaParaMin h2) - (horaParaMin h1)

tempoEspera :: Viagem -> Int
tempoEspera v = let (p,c) = horaPartidaChegada v
                    totalViagem = (horaParaMin c) - (horaParaMin p)
                in totalViagem - tempoEfetivo v

tempoViagem :: Viagem -> Int
tempoViagem v = let (p,c) = horaPartidaChegada v
                in  (horaParaMin c) - (horaParaMin p)


--Exercicio #2

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

type Poligonal = [Ponto]

dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ( (x1 - x2)^2 + (y1 - y2)^2)

comprimento :: Poligonal -> Double
comprimento [x] = 0
comprimento (h:i:t) = (dist h i) + comprimento (i:t)

eFechada :: Poligonal -> Bool
eFechada l = head l ==last l

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:[])=[]
triangula (p1:p2:p3:ps) = (Triangulo p1 p2 p3) : (triangula (p1:p3:ps))

--Questao 3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

--(a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail a b [] = [(a,[Email b])]
acrescEmail a b ((x,t):r) |a == x = (x,(ac (Email b) t)):r
                          |otherwise = (x,t):acrescEmail a b r

ac :: Contacto -> [Contacto] -> [Contacto]
ac a [] = [a]
ac a (h:t) = (a:h:t)

-- o resto esta no outro pc if not fazer dps




-- Questao 4

type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
          deriving Show

type TabDN = [(Nome,Data)]

--(a)
procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing
procura n ((x,(D a b c)):t) = if n == x then Just (D a b c)
                                        else procura n t

--(b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade d n [] = Nothing
idade (D a b c) n ((x,(D d e f)):t) |n == x && b<e = Just (c -f-1)
                                    |n == x && b == e && a<d = Just (c-f-1)
                                    |n == x && b == e && a>d = Just (c-f)
                                    |n == x && b == e && a == d = Just (c-f)
                                    |n ==  x && b>e = Just (c-f)
                                    |otherwise = idade (D a b c ) n t

--(c)
anterior ::  Data -> Data -> Bool
anterior (D a b c) (D x y z) |z>c = True
                             |z==c && b==y && a<x = True
                             |z==c && b<y = True
                             |otherwise = False

--(d)
ordena :: TabDN -> TabDN
ordena [] = []
ordena ((x,d1):t) = insere (x,d1) (ordena t)
                  where insere d [] = [d]
                        insere (x,d) ((y,a):t) = if anterior d a then ((x,d):(y,a):t)
                                                                 else (y,a):insere (x,d) t

--(e)
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade d (h:t) = idades d [ordena (h:t)]
                where idades d [] = []
                      idades d (h:t) = idade' d h ++ idades d t

idade' :: Data -> TabDN -> [(Nome,Int)]
idade' d [] = []
idade' (D a b c) ((x,(D d e f)):t) |b<e =  (x,(c -f-1)):idade' (D a b c) t
                                   |b == e && a<d = (x,(c-f-1)) : idade' (D a b c) t
                                   |b == e && a>d = (x,(c-f)) : idade' (D a b c) t
                                   |b == e && a == d = (x,(c-f)):idade' (D a b c) t
                                   |b>e = (x,(c-f)) : idade' (D a b c) t
                                   |otherwise = idade' (D a b c )  t


--Questao 5
data Movimento = Credito Float | Debito Float
                deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
                deriving Show

--(a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext v []) a = []
extValor (Ext v ((d,ds,(Credito f)):t)) a |f> a =  (Credito f): extValor (Ext v t) a
                                         |otherwise = extValor (Ext v t) a
extValor (Ext v ((d,ds,(Debito f)):t)) a |f> a =  (Debito f): extValor (Ext v t) a
                                        |otherwise = extValor (Ext v t) a

--(b)
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext v []) s = []
filtro (Ext v ((d,ds,m):t)) s = if elem ds s then (d,m):filtro (Ext v t) s
                                             else filtro (Ext v t) s

--(c)
creDeb :: Extracto -> (Float,Float)
creDeb e = let a = contaCred (extValor e 0)
               b = contaDeb (extValor e 0)
           in (a,b)

contaCred :: [Movimento] -> Float
contaCred [] = 0
contaCred ((Credito d):t) = d+contaCred t
contaCred (h:t) = contaCred t

contaDeb :: [Movimento] -> Float
contaDeb [] = 0
contaDeb ((Debito d):t) = d+contaDeb t
contaDeb (h:t) = contaDeb t

--(d)
