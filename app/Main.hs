--import Lib

--main :: IO ()

------------------------------------------Modify a List AVEC PATRONS
--ajouterUn [] = []
--ajouterUn (x : xs) = (x+1) : ajouterUn xs
----ajouterUn = map (+ 1)
--main = print(ajouterUn [7, 2, 3])

------------------------------------------Type de donnée : Fonction
--appliquer fx = fx
--inc :: Int -> Int
--inc y = y + 1
--main = print(inc 5)

------------------------------------------Lambas
--appliquer ( \x -> 3 * x) 
--main = print (appliquer 5)

------------------------------------------Addition
--add :: Int -> Int -> Int
--add x y = x + y
--main = print(add 5 2)

------------------------------------------Head - Tail
--liste = [2,4,6,8]
--x = head liste -- x == 2
--xs = tail liste  -- xs = [4,6,8]
--main = print(x, xs)

-----------------------------------------Fonction composé
--f n = n + 1
--g n = 2*n - 1
--h = f . g  -- h is the composition of f and g
--main = print(h 3) --or main = print(f(g(3)))

------------------------------------------Patrons
--fac 0 = 1
--fac n = n * (fac(n-1))
--main = print(fac(-1))

------------------------------------------App. Partielle
--add x y = x + y
--inc = add 1
--main = print(inc 3)

------------------------------------------Trick $
--Permet d'éviter les parenthèses : inc $ 3 + 1 ⟺ inc ( 3 + 1 )



        ----------- Labo 1 -----------------
-----Fibonacci avec gardes
--fibo :: Int -> Int
--fibo n | n == 0 = 0 --n + fibo (n - 1)
--       | n == 1 = 1
--       | otherwise = fibo(n-1) + fibo(n-2)
--main = print(fibo 8)

-----Fibonacci AVEC PATRONS
--fibo 0 = 0
--fibo 1 = 1
--fibo n = fibo (n-1) + fibo (n-2)
--main = print(fibo 8)

-----Faire liste de fibonnaci de la n-ieme position
--listeNFibo :: Int -> [Int]
--listeNFibo n = f n 0
--  where
--    f n compteur
--      | n == compteur = []
--      | otherwise = fibo compteur : f n (compteur + 1)
--main = print(listeNFibo 5)

----------------------Append number to list
--func a [] = [a]
--func n (x:xs) = x : func n xs
--main = print(func 5 [2,4,8])

---------Labo 2
---calculer la somme d'une liste d'INT
--calsomme [] = 0
--calsomme (x:xs) = x + calsomme xs
--main = print(calsomme [1,2,4,5])

---calculer la k-iemme puissance de chaque element de la liste et faire la somme des resultat
--somk::Double->[Double]->Double
--somk k [] = 0
--somk k (x:xs) = (x**k) + somk k xs
--main = print(somk 2 [3,5])

-----------------Map (prend une liste, retourn la liste avec le m^ nb d'ele mais modifiés)
--maper = map(\(x,y) -> x+y)
--main = print(maper [(1,2), (2,3), (3,4)])

-----------------Map (prend une liste, retourn la liste avec le m^ nb d'ele mais modifiés)
--filtrer :: [Integer] -> [Integer]
--filtrer = filter(>= 2)
--main = print(filtrer [0, -2, 4, 3 , 2, -8])

---
--pNorme :: Double -> [Double] -> Double
--pNorme k [] = 0
--pNorme k (x:xs) = (x**k) + pNorme k xs
--main = print(sqrt(pNorme 2 [3,4]))

--app :: (t1 -> t2) -> t1 -> t2
--app f x = f x
--main = print(app pNorme 2 [2,4])


-- noms :
--

module Main where

import Data.List
import Debug.Trace
import System.Environment

_POSITION_ARG_NOM_FICHIER = 0
_POSITION_ARG_PRECISION = 1

_MSSG_AIRE = "Aire : "
_MSSG_ERREUR_NOMBRE_ARGUMENT = "Il doit y avoir 2 arguments sur la ligne de commande."

_PRECISION_MIN = 2
_PRECISION_MAX = 10000


-- fonction pour lire un Int dans une chaine de carateres ne contenant qu'un seul Int.
-- @param String la chaine de caracteres a lire.
extraireInt :: String -> Int
extraireInt = read

-- transforme un Int en Double.
-- @param Int la valeur a transformer.
castIntDouble :: Int -> Double
castIntDouble = fromIntegral

-- borne inferieurement la precision en utilisant la constante de precision minimum.
-- Si la valeur est plus petite que la borne, alors elle devient egale a la borne.
-- @param Int la valeur a borner.
bornerInfPrecision :: Int -> Int
bornerInfPrecision = max _PRECISION_MIN

-- borne superieurement la precision en utilisant la constante de precision maximum.
-- Si la valeur est plus grande que la borne, alors elle devient egale a la borne.
-- @param Int la valeur a borner.
bornerSupPrecision :: Int -> Int
bornerSupPrecision = min _PRECISION_MAX

-- maintient une valeur de precision entre ces deux bornes.
-- @param Int la valeur a borner
bornerPrecision :: Int -> Int
bornerPrecision = bornerInfPrecision . bornerSupPrecision

-- extrait la valeur de 'precision' d'une chaine de caracteres et la transforme en Double.
-- la valeur extraite est aussi borne entre les valeurs permise.
-- @param String la chaine de caractere contenant un Int.
extrairePrecision :: String -> Double
extrairePrecision = castIntDouble . bornerPrecision . extraireInt

main =
    do argv <- getArgs
       let argc = length argv
       nomFichier <- if _POSITION_ARG_NOM_FICHIER < argc
                     then return ( argv !! _POSITION_ARG_NOM_FICHIER )
                     else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       precisionS <- if _POSITION_ARG_PRECISION < argc
                     then return ( argv !! _POSITION_ARG_PRECISION )
                     else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       contenuFichier <- readFile nomFichier
       let precision = extrairePrecision precisionS
       ---
       let formes = map parseurForme (lines contenuFichier) -----
       let parsedShapes = map calculerCoordonnees formes  -----
       ---
       let aire = traitement contenuFichier precision
       putStr ( _MSSG_AIRE ++ show aire )
       ------
       putStr (show formes)
       putStr (show parsedShapes)

--------------------------------------------------------------
-- Votre code commence ici.
mettreArray = lines

data Forme = Carre Double Double Double
           | Rectangle Double Double Double Double
           | Cercle Double Double Double
           | Ellipse Double Double Double Double Double
           deriving (Show)
           
           
calculerCoordonnees :: Forme -> (Double, Double, Double, Double)
calculerCoordonnees (Carre cx cy t) = (cx - t / 2, cy - t / 2, cx + t / 2, cy + t / 2)
calculerCoordonnees (Rectangle cx cy b h) = (cx - b / 2, cy - h / 2, cx + b / 2, cy + h / 2)
calculerCoordonnees (Cercle cx cy r) = (cx - r, cy - r, cx + r, cy + r)
calculerCoordonnees (Ellipse cx cy dfx dfy g) = (cx - g/2, cy - g/2, cx + g/2, cy + g/2)

calculerListeCoordonnees :: [Forme] -> [(Double, Double, Double, Double)]
calculerListeCoordonnees = map calculerCoordonnees

parseurForme :: String -> Forme
parseurForme str = case words str of
  ["carre", x, y, t] -> Carre (read x) (read y) (read t)
  ["rectangle", x, y, b, h] -> Rectangle (read x) (read y) (read b) (read h)
  ["cercle", x, y, r] -> Cercle (read x) (read y) (read r)
  ["ellipse", x, y, dx, dy, g] -> Ellipse (read x) (read y) (read dx) (read dy) (read g)
  _ -> error "Forme invalide"

-- fonction calculant l'aire de la forme complexe.
-- @param String contient la description de la forme complexe sous forme d'une liste de forme simple.
-- @param Double la precision utilisee pour la methode numerique.
traitement :: String -> Double -> Double
traitement contenuFichier precision = 0.0




