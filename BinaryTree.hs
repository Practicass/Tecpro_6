module BinaryTree where
import Data.List


data BinTree t = Hoja t (BinTree t) (BinTree t) | ArbVacio

empty :: BinTree t
empty = ArbVacio

leaf :: t -> BinTree t
leaf t = Hoja t (empty) (empty)

tree :: t -> BinTree t -> BinTree t -> BinTree t
tree t lc rc = Hoja t (lc) (rc) 


size ::  BinTree t -> Integer
size ArbVacio = 0
size (Hoja t (lc) (rc)) = 1 + size lc + size rc

add :: Ord t => BinTree t -> t -> BinTree t
add ArbVacio  x = leaf x
add (Hoja t (lc) (rc)) x =  if x < t   
                            then  Hoja t (add lc x) (rc) 
                            else  Hoja t (lc) (add rc x)


buildAux ::  Ord t => BinTree t -> [t] -> BinTree t
buildAux  arb [] = arb
buildAux  arb (x:xs) =  buildAux  (add arb x) xs

build ::  Ord t => [t] -> BinTree t
build x = buildAux empty x

mediana :: [t] -> Int
mediana x =  (div ((length x)) 2) 

mediana2 :: [t] -> t
mediana2 [x] = x 
mediana2 [x,y] = y
mediana2 (x:xs) = mediana2 (init xs)


ordenarAux :: Ord t =>  ([t],[t]) -> [t]
ordenarAux (x,[]) = x
ordenarAux ([],y) = y
ordenarAux ([x],y) = [x]++ordenarAux (splitAt (mediana y) y)
ordenarAux (x,[y]) =  ordenarAux (splitAt (mediana x) (delete (mediana2 x) (x))) ++ [y] 
ordenarAux (x,[y1,y2]) =   [mediana2 x] ++ ordenarAux (splitAt (mediana x) (delete (mediana2 x) (x))) ++ [y2,y1]
ordenarAux (x,y) =  [mediana2 x] ++ ordenarAux (splitAt (mediana x) (delete (mediana2 x) (x))) ++  ordenarAux (splitAt (mediana y) y)

ordenar :: Ord t =>  [t] -> [t]
ordenar x = [mediana2 x] ++ ordenarAux (splitAt (mediana x) (delete (mediana2 x) (x)))

buildBalanced ::  Ord t => [t] -> BinTree t
buildBalanced x =  build (ordenar (sort x))

preorder :: Ord t => BinTree t -> [t]
preorder ArbVacio = []
preorder( Hoja x (lc) (rc) )= [x] ++ preorder lc ++ preorder rc

postorder :: Ord t => BinTree t -> [t]
postorder ArbVacio = [] 
postorder (Hoja x (lc) (rc)) = postorder lc ++ postorder rc ++ [x]

inorder :: Ord t => BinTree t -> [t]
inorder ArbVacio = [] 
inorder (Hoja x (lc) (rc)) = inorder lc ++ [x] ++ inorder rc


balance :: Ord t => BinTree t -> BinTree t
balance ArbVacio = ArbVacio
balance x = buildBalanced (inorder x)


between ::  Ord t => BinTree t -> t -> t -> [t]
between ArbVacio _ _ = []
between (Hoja x (lc) (rc)) a b  | x < a = between rc a b
                                | x > b = between lc a b
                                | otherwise = [x] ++ between lc a b ++ between rc a b


mostrarAux :: (Show t) => BinTree t -> String -> String
mostrarAux  (ArbVacio)  tab = "<>"
mostrarAux (Hoja t (ArbVacio) (ArbVacio)) tab = show t 
mostrarAux (Hoja t (lc) (rc)) tab = show t ++ "\n" ++ (tab ++ "  ") ++  "|- " ++ mostrarAux lc (tab ++ "  ") ++  "\n" ++ (tab ++ "  ") ++ "|- " ++ mostrarAux rc (tab ++ "  ")

instance (Show t) => Show (BinTree t) where
    show t = mostrarAux t ""
