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
add (Hoja t (lc) (rc)) x =  if x <= t   
                            then  Hoja t (add lc x) (rc) 
                            else  Hoja t (lc) (add rc x)



buildAux ::  Ord t => BinTree t -> [t] -> BinTree t
buildAux  arb [] = arb
buildAux  arb (x:xs) =  buildAux  (add arb x) xs

build ::  Ord t => [t] -> BinTree t
build x = buildAux empty x




mostrarAux :: (Show t) => BinTree t -> String -> String
mostrarAux  (ArbVacio)  tab = "<>"
mostrarAux (Hoja t (ArbVacio) (ArbVacio)) tab = show t 
mostrarAux (Hoja t (lc) (rc)) tab = show t ++ "\n"++(tab++"  ")++ "|- " ++ mostrarAux lc (tab++"  ") ++  "\n"++(tab++"  ")++ "|- "++ mostrarAux rc (tab++"  ")

instance (Show t) => Show (BinTree t) where
    show t = mostrarAux t ""
