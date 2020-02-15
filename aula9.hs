
import System.Random
import Data.Char
import System.Exit
import Control.Monad

--Questao 1 
--(a)
bingo' :: IO [Int]
bingo' = bingoA []

bingoA :: [Int] -> IO [Int]
bingoA l |length l == 90 = return l
         |otherwise = do a <- randomRIO(1,90)
                         print a 
                         getChar
                         let nl = if elem a l 
                                  then l
                                  else a:l in bingoA l

bingo :: IO [Int]
bingo = bingoX [1..90]

bingoX :: [Int] -> IO [Int]
bingoX [] = return []
bingoX l = do p <- randomRIO (0,(length l)-1)
           -- let x = l !! p
            --let y = (take p l) ++ (tail (drop p l))
              xs <- bingoX ((take p l) ++ (tail (drop p l)))
              return ((l!!p):xs)


-- (b)
secreto :: IO (Int,Int,Int,Int)
secreto = do c1 <- randomRIO (0,9)
             c2 <- randomRIO (0,9)
             c3 <- randomRIO (0,9)
             c4 <- randomRIO (0,9)
             return (c1,c2,c3,c4)

meuCodigo :: IO (Int,Int,Int,Int)
meuCodigo = do c1 <- getChar
               c2 <- getChar
               c3 <- getChar
               c4 <- getChar
               return (digitToInt c1,digitToInt c2,digitToInt c3,digitToInt c4)

mesmaPosicao :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int
mesmaPosicao (a1,a2,a3,a4) (b1,b2,b3,b4) = length (filter (==True) [a1==b1,a2==b2,a3==b3,a4==b4])

pertenceLista :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int
pertenceLista (a1,a2,a3,a4) (b1,b2,b3,b4) = undefined 

mastermind :: IO ()
mastermind = do s <- secreto
                forever $ do codigo <- meuCodigo
                             let certo = mesmaPosicao codigo s
                             if certo == 4 then do putStrLn ("\nGG")
                                                   exitSuccess
                                           else putStrLn ("\nCorretos:" ++ (show certo))

--Questao 2
data Aposta = Ap [Int] (Int,Int)
--(a)
valida :: Aposta -> Bool
valida (Ap (h:t) (x,y)) |aux h t && x >0 && y >0 && x<10 && y <10 && x/=y && length (h:t) == 5 = True
                        |otherwise = False
                        where aux h [] = True
                              aux h (x:t) |h>0 && h<51 && h /= x = aux x t
                                          |otherwise = False

--(b)
comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l (x,y)) (Ap ls (m,n)) = let a = numerosIguas (Ap l (x,y)) (Ap ls (m,n))
                                        b = estrelasIguais (Ap l (x,y)) (Ap ls (m,n))
                                    in (a,b)

numerosIguas :: Aposta -> Aposta -> Int
numerosIguas (Ap [] l) (Ap x s) = 0
numerosIguas (Ap (h:t) l ) (Ap x s) |elem h x = 1+ numerosIguas (Ap t l) (Ap x s)
                                    |otherwise = numerosIguas (Ap t l) (Ap x s)

estrelasIguais :: Aposta -> Aposta -> Int
estrelasIguais (Ap l (x,y)) (Ap s (m,n)) = aux x (m,n) + aux y (m,n)
                                         where aux h (x,y) |h == x || h==y = 1
                                                           |otherwise = 0

--(c)
--(i)
instance Eq Aposta where
    (==) a b = (comuns a b) == (5,2) 

--(ii)
premio :: Aposta -> Aposta -> Maybe Int
premio a b |comuns a b == (5,2) = Just 1
           |comuns a b == (5,1) = Just 2
           |comuns a b == (5,0) = Just 3
           |comuns a b == (4,2) = Just 4
           |comuns a b == (4,1) = Just 5
           |comuns a b == (4,0) = Just 6
           |comuns a b == (3,2) = Just 7
           |comuns a b == (2,2) = Just 8
           |comuns a b == (3,1) = Just 9
           |comuns a b == (3,0) = Just 10
           |comuns a b == (1,2) = Just 11
           |comuns a b == (2,1) = Just 12
           |comuns a b == (2,0) = Just 13
           |otherwise =  Nothing

--(d)
--(i)
leAposta :: IO Aposta
leAposta = do putStrLn ("Introduzir Aposta:\n")
              n1 <- getLine
              n2 <- getLine
              n3 <- getLine
              n4 <- getLine
              n5 <- getLine
              e1 <- getLine
              e2 <- getLine
              let a = [read n1 :: Int,read n2 :: Int,read n3 :: Int,read n4 :: Int,read n5 :: Int]
              let b = (read e1 :: Int,read e2 :: Int)
              apostaValida (Ap a b)

apostaValida :: Aposta -> IO Aposta
apostaValida (Ap a b) |valida (Ap a b) = do putStrLn ((show a) ++ " " ++ (show b))
                                            return (Ap a b)
               |otherwise = do putStrLn ("Aposta Inválida,tente novamente:\n")
                               leAposta

--(ii)
joga :: Aposta -> IO ()
joga b = do a <-leAposta
            print (comuns a b)

--(e)
geraChave :: IO Aposta
geraChave = do a <- geraNum
               b <- geraEst
               putStrLn ((show a) ++ " " ++ (show b))
               return (Ap a b)

geraNum :: IO [Int]
geraNum = geraNumA [1..50]

geraNumA :: [Int] -> IO [Int]
geraNumA [] = return []
geraNumA l = do p <- randomRIO (0,(length l)-1)
           -- let x = l !! p
            --let y = (take p l) ++ (tail (drop p l))
                xs <- bingoX ((take p l) ++ (tail (drop p l)))
                return (take 5((l!!p):xs))

geraEst :: IO (Int,Int)
geraEst = do a <- randomRIO (1,9)
             b <- randomRIO (1,9)
             if geraEstA (a,b) then do return (a,b)
                               else geraEst
             where geraEstA (a,b) = a/=b

--(f)
main :: IO ()
main = do ch <- geraChave
          ciclo ch

menu :: IO String
menu = do { putStrLn menutxt
          ; putStr "Opcao: "
          ; c <- getLine
          ; return c
          }
        where menutxt = unlines ["",
                                "Apostar ........... 1",
                                "Gerar nova chave .. 2",
                                "",
                                "Sair .............. 0"]

ciclo :: Aposta -> IO ()
ciclo c = do o <- menu
             if o == "1" 
             then do a <- joga c
                     putStr ((show a) ++ "º Prémio\n")
                     ciclo c
             else if o == "2" 
                  then do b <- geraChave
                          ciclo b
                  else do exitSuccess