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
       let listeFormes = map parseurForme (lines contenuFichier) -----
       let listeCoordRectEnglobantFormesSimples = map calculerCoordonnees listeFormes  -----
       let minX = minimum (map (\(x,y,z,w) -> x) listeCoordRectEnglobantFormesSimples)
       let minY = minimum (map (\(x,y,z,w) -> y) listeCoordRectEnglobantFormesSimples)
       let maxX = maximum (map (\(x,y,z,w) -> z) listeCoordRectEnglobantFormesSimples)
       let maxY = maximum (map (\(x,y,z,w) -> w) listeCoordRectEnglobantFormesSimples)
       let coordGrandRect = (minX, minY, maxX, maxY)
       let bhPetitRect = calculerBHPetitRect coordGrandRect precision
       let coordCentrePetitRect = calculerCoordCentrePetitRect bhPetitRect minX minY precision
       let listeVrai = map (\coordCentrePetitRect -> estInterieur coordCentrePetitRect listeFormes) coordCentrePetitRect
       let q = fromIntegral (length (filter (==True) listeVrai)) :: Double
       ---
       let aire = traitement bhPetitRect q
       ------
--       putStrLn ("Liste de Formes à partir du fichier transformé->    " ++ show listeFormes)
--       putStrLn ("(3.1.1) Liste contenant toutes les coords des rect englobants des formes simples->    " ++show listeCoordRectEnglobantFormesSimples)
--       putStrLn ("(3.1.2) Coords du grand rectangle extraites de la liste précédente->    " ++ show coordGrandRect)
--       putStrLn ("(3.2) Base et Hauteur d'un petit rect->    " ++ show bhPetitRect)
--       putStrLn ("(3.2) Liste contenant les coords du centre de chaque petits rect a TESTER->    " ++ show coordCentrePetitRect)
--       putStrLn ("(3.2) Liste des Vrais dans la fct estInterieur->    " ++ show listeVrai)
       putStrLn ( _MSSG_AIRE ++ show aire )
--------------------------------------------------------------

data Forme = Carre Double Double Double
           | Rectangle Double Double Double Double
           | Cercle Double Double Double
           | Ellipse Double Double Double Double Double
           | Polygone [(Double, Double)]
           deriving (Show)

-- | Cette fonction calcule les coordonnees d'une forme donnee et renvoie un tuple contenant les coordonnées minimales et maximales de la forme
-- @param Forme une forme
-- @return un tuple de 4 valeurs représentant les coordonnées minimales et maximales en x et y
calculerCoordonnees :: Forme -> (Double, Double, Double, Double)
calculerCoordonnees (Carre cx cy t) = (cx - t / 2, cy - t / 2, cx + t / 2, cy + t / 2)
calculerCoordonnees (Rectangle cx cy b h) = (cx - b / 2, cy - h / 2, cx + b / 2, cy + h / 2)
calculerCoordonnees (Cercle cx cy r) = (cx - r, cy - r, cx + r, cy + r)
calculerCoordonnees (Ellipse cx cy dfx dfy g) = (cx - g/2, cy - g/2, cx + g/2, cy + g/2)
calculerCoordonnees (Polygone points) = (minimum pwx, minimum pwy, maximum pwx, maximum pwy)
  where (pwx, pwy) = unzip points
--  where xs = map fst points -- extract x values from pts
--        ys = map snd points -- extract y values from pts
--        minX = minimum xs   -- find min x value
--        minY = minimum ys   -- find minm y value
--        maxX = maximum xs   -- find max x value
--        maxY = maximum ys   -- finds max y value.

-- | Convertit une liste de chaînes en une liste de coordonnées de points (x,y)
--   @param Une liste de chaînes de caractères représentant les coordonnées du polygone.
--   @return Une liste de tuples (x, y) représentant les coordonnées du polygone.
parseurPolygoneCoords :: [String] -> [(Double, Double)]
parseurPolygoneCoords [] = []
parseurPolygoneCoords (x:y:rest) = (read x, read y) : parseurPolygoneCoords rest
parseurPolygoneCoords _ = error "Coordonnées de polygone invalides"

-- | calcule les coordonnées pour chaque forme d'une liste de formes
-- @param [Forme] une liste de formes
-- @return [(Double, Double, Double, Double)] une liste de tuples représentant les coordonnées calculées pour chaque forme
calculerListeCoords :: [Forme] -> [(Double, Double, Double, Double)]
calculerListeCoords = map calculerCoordonnees

-- fonction pour parser une chaine de caracteres et creer une forme correspondante.
-- @param String la chaine de caracteres a parser.
parseurForme :: String -> Forme
parseurForme str = case words str of
  ["carre", x, y, t] -> Carre (read x) (read y) (read t)
  ["rectangle", x, y, b, h] -> Rectangle (read x) (read y) (read b) (read h)
  ["cercle", x, y, r] -> Cercle (read x) (read y) (read r)
  ["ellipse", x, y, dx, dy, g] -> Ellipse (read x) (read y) (read dx) (read dy) (read g)
  ("polygone":coords) -> Polygone (parseurPolygoneCoords coords)

-- | fonction pour calculer la base et la hauteur d'un petit rectangle
-- @param (Double, Double, Double, Double) les coordonnees du grand rectangle.
-- @param Double la valeur de precision.
-- @return return la base et hauteur en forme de tuple
calculerBHPetitRect :: (Double, Double, Double, Double) -> Double -> (Double, Double)
calculerBHPetitRect (x, y, w, z) p = ((w - x)/p, (z - y)/p)

-- | fonction pour calculer les coordonnees du centre d'un petit rectangle.
-- @param (Double, Double) la base et la hauteur du petit rectangle.
-- @param Double la coordonnee x du coin inferieur gauche du petit rectangle.
-- @param Double la coordonnee y du coin inferieur gauche du petit rectangle.
-- @param Double la valeur de precision.
-- @ return une liste de tuples, où chaque tuple représente les coordonnées du centre d'un petit rectangle.
calculerCoordCentrePetitRect :: (Double, Double) -> Double -> Double -> Double -> [(Double, Double)]
calculerCoordCentrePetitRect (b, h) minX minY precision = [(minX + (b * (i + 0.5)), minY + (h * (j + 0.5))) | i <- [0..precision-1], j <- [0..precision-1]]


-- | Vérifie si un point est à l'intérieur d'au moin une forme
-- @param coords Les coordonnées du point à tester.
-- @param formes La liste de formes à tester.
-- @return Bool True si le point est à l'intérieur d'au moins une forme, False sinon.
estInterieur :: (Double, Double) -> [Forme] -> Bool
estInterieur coords = any (formePred coords)
  where formePred coords (Carre cx cy t) = carre coords (cx, cy) t
        formePred coords (Rectangle cx cy b h) =rectangle coords (cx, cy) (b, h)
        formePred coords (Cercle cx cy r) = cercle coords (cx, cy) r
        formePred coords (Ellipse cx cy r1 r2 a) = ellipse coords (cx, cy) r1 r2 a
        formePred coords (Polygone points) = polygone coords points

-- | Vérifie si le point (xi, yi) est contenu dans un carré
-- @param xi x de la coord
-- @param yi y de la coord
-- @param cx x du centre du carré
-- @param cy y du centre du carré
-- @param t taille du côté du carré
-- @return True si le point est contenu dans le carré, False sinon.
carre :: (Double, Double) -> (Double, Double) -> Double -> Bool
carre (xi, yi) (cx, cy) t = (cx - t/2 <= xi) && (xi <= cx + t/2) && (cy - t/2 <= yi) && (yi <= cy + t/2)

-- | Vérifie si le point (xi, yi) est contenu dans un rectangle centré
-- @param xi x de la coord
-- @param yi y de la coord
-- @param cx x du centre du rectangle
-- @param cy y du centre du rectangle
-- @param b base du rectangle
-- @param h hauteur du rectangle
-- @return True si le point est contenu dans le rectangle, False sinon.
rectangle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
rectangle (xi, yi) (cx, cy) (b, h) = (cx - b/2 <=xi) && (xi <= cx + b/2) && (cy - h/2 <= yi) && (yi <= cy + h/2)

-- | Vérifie si le point (xi, yi) est contenu dans un cercle
-- @param xi x de la coord
-- @param yi y de la coord
-- @param cx x du centre du cercle
-- @param cy y du centre du cercle
-- @param r rayon
-- @return True si le point est contenu dans le cercle, False sinon.
cercle :: (Double, Double) -> (Double, Double) -> Double -> Bool
cercle (xi, yi) (cx, cy) r = (xi - cx)^2 + (yi - cy)^2 <= r^2

-- | Vérifie si le point (xi, yi) est contenu dans une ellipse
-- @param xi x de la coord
-- @param yi y de la coord
-- @param cx x du centre de l'ellipse
-- @param cy y du centre de l'ellipse
-- @param r1 rayon horizontal
-- @param r2 rayon vertical
-- @param a angle de rotation de l'ellipse en radians
-- @return True si le point est contenu dans l'ellipse, False sinon.
ellipse :: (Double, Double) -> (Double, Double) -> Double -> Double -> Double -> Bool
ellipse (xi, yi) (cx, cy) r1 r2 a = norme ((cx + r1, cy + r2) `subtractVec` (xi, yi)) + norme ((cx - r1, cy -r2) `additionVec` (xi, yi)) <= a

-- | Cette fonction ajoute deux vecteurs représentés par des paires ordonnées.
-- @param (a,b) les coordonnées du premier vecteur
-- @param (a,b) les coordonnées du deuxième vecteur
-- @return une paire ordonnée représentant le résultat de l'addition des deux vecteurs
additionVec :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
additionVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- | Cette fonction calcule la différence entre deux vecteurs
-- @param (a,b) le premier vecteur
-- @param (a,b) le deuxième vecteur
-- @return un tuple représentant le résultat de la différence entre les deux vecteurs
subtractVec :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
subtractVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- | Multiplie deux vecteurs à deux dimensions
-- @param (Double, Double) Le premier vecteur représenté par un tuple.
-- @param (Double, Double) Le deuxième vecteur représenté par un tuple.
-- @return Double Le produit scalaire des deux vecteurs.
multVec :: (Double, Double) -> (Double, Double) -> Double
multVec (x1, y1) (x2, y2) = x1*x2 + y1*y2

-- | Transpose un vecteur à deux dimensions
-- @param (Double, Double) Le vecteur représenté par un tuple.
-- @return (Double, Double) Le vecteur transposé représenté par un tuple.
transposeVec :: (Double, Double) -> (Double, Double)
transposeVec (x1, y1) = (y1, -1 * x1)

-- | Calcule la norme d'un vecteur à deux dimensions représenté par un tuple (x, y).
-- @param (Double, Double) Le tuple représentant les coordonnées du vecteur.
-- @return Double La norme du vecteur.
norme :: (Double, Double) -> Double
norme (x1, y1) = sqrt(x1*x1 + y1*y1)

-- | Détermine si un point est à l'interieur d'un polygone défini par une liste de points.
-- @param (Double, Double) Le point à tester
-- @param [(Double, Double)] La liste des points qui définissent le polygone.
-- @return Bool True si le point est à l'intérieur du polygone, False sinon
polygone :: (Double, Double) -> [(Double, Double)] -> Bool
--polygone (x, y) pointsPoly = odd (length (filter intersect aretes))
polygone coords pointsPoly = all (intersecte coords) aretes
  where
    aretes = zip pointsPoly (tail (cycle pointsPoly))
    intersecte (x,y) ((x1,y1),(x2,y2)) = (((x,y) `subtractVec` (x1,y1)) `multVec` transposeVec ((x1,y1) `subtractVec` (x2,y2))) >= 0
--    intersect ((x1,y1),(x2,y2)) = (y1 > y) /= (y2 > y) && x < (x2 - x1) * (y - y1) / (y2 - y1) + x1


-- | fonction pour calculer l'aire d'un petit rectangle.
-- @param (Double, Double) la base et la hauteur du petit rectangle.
-- @param Double le nombre de petits rectangles qui contiennent un point de la forme.
-- @return Double le calcul de laire final de la forme complexe
traitement :: (Double, Double) -> Double -> Double
traitement (b, h) q = b * h * q
