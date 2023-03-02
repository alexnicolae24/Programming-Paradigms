module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))


{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node a) = S.singleton a
nodes (Overlay graph1 graph2) = S.union (nodes graph1) (nodes graph2)
nodes (Connect graph1 graph2) = S.union (nodes graph1) (nodes graph2)


{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node a) = S.empty
edges (Overlay graph1 graph2) = S.union (edges graph1) (edges graph2)
edges (Connect graph1 graph2) = S.union (edges graph1) (S.union (edges graph2) (S.cartesianProduct (nodes graph1) (nodes graph2)))

                                                         



{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node graph = case graph of
                            Empty -> S.empty
                            (Node a) -> S.empty 
                            (Overlay graph1 graph2) -> S.union (oN graph1) (oN graph2)
                            (Connect graph1 graph2) -> if (S.member node (nodes graph1) == False) then S.empty else S.union (oN graph1) (nodes graph2)
                        where
                            oN = outNeighbors node


{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node graph = case graph of
                            Empty -> S.empty
                            (Node a) -> S.empty 
                            (Overlay graph1 graph2) -> S.union (iN graph1) (iN graph2)
                            (Connect graph1 graph2) -> if (S.member node (nodes graph2) == False) then S.empty else S.union (iN graph2) (nodes graph1)
                        where
                            iN = inNeighbors node

{-
    *** TODO ***

    Instanțiați clasa Num cu tipul (AlgebraicGraph a), astfel încât:
    - un literal întreg să fie interpretat ca un singur nod cu eticheta egală
      cu acel literal
    - operația de adunare să fie intepretată ca Overlay
    - operația de înmulțire să fie interpretată drept Connect.

    Celelalte funcții din clasă nu sunt relevante. Veți obține warning-uri
    pentru neimplementarea lor, dar puteți să le ignorați.

    După instanțiere, veți putea evalua în consolă expresii ca:

    > 1 :: AlgebraicGraph Int
    Node 1
    
    > 1*(2+3) :: AlgebraicGraph Int
    Connect (Node 1) (Overlay (Node 2) (Node 3))
-}
instance Num a => Num (AlgebraicGraph a) where
    fromInteger a = (Node(fromInteger a))
    (+) x y = Overlay x y
    (*) x y = Connect x y

{-
    *** TODO ***

    Instanțiați clasa Show cu tipul (AlgebraicGraph a), astfel încât
    reprezentarea sub formă de șir de caractere a unui graf să reflecte
    expresiile aritmetice definite mai sus. Puteți pune un nou rând de paranteze
    la fiecare subexpresie compusă.

    Exemple:

    > Node 1
    1

    > Connect (Node 1) (Overlay (Node 2) (Node 3))
    (1*(2+3))
-}
instance Show a => Show (AlgebraicGraph a) where
    show graph = case graph of
                Empty -> "NULL"
                Node a -> show a
                Overlay graph1 graph2 -> "(" ++ show graph1 ++ "+" ++ show graph2 ++")"
                Connect graph1 graph2 -> "(" ++ show graph1 ++ "*" ++ show graph2 ++")"


{-
    *** TODO ***

    Observați că instanța predefinită de Eq pentru tipul (AlgebraicGraph a)
    nu surprinde corect egalitatea a două grafuri, deoarece același graf
    conceptual poate avea două descrieri simbolice diferite.
    
    Prin urmare, instanțiați clasa Eq cu tipul (AlgebraicGraph a), astfel încât
    să comparați propriu-zis mulțimile de noduri și de arce.

    Exemple:

    > Node 1 == 1
    True

    > Node 1 == 2
    False

    > angle == 1*2 + 1*3
    True

    > triangle == (1*2)*3
    True
-}
instance Ord a => Eq (AlgebraicGraph a) where
    g1 == g2 = (nodes g1 == nodes g2) && (edges g1 == edges g2)
    --g1 == g2 = True
   


{-
    *** TODO ***

    Extinde un graf existent, atașând noi subgrafuri arbitrare în locul nodurilor
    individuale. Funcția primită ca prim parametru determină această
    corespondență între noduri și subgrafuri. Observați că tipul etichetelor
    noi (b) poate diferi de al etichetelor vechi (a).

    Exemplu:

    > extend (\n -> if n == 1 then 4+5 else Node n) $ 1*(2+3)
    ((4+5)*(2+3))
-}
extend :: (a -> AlgebraicGraph b) -> AlgebraicGraph a -> AlgebraicGraph b
extend f graph = case graph of
                    Empty -> Empty
                    Node a -> f a
                    Overlay graph1 graph2 -> Overlay (extend f graph1) (extend f graph2)
                    Connect graph1 graph2 -> Connect (extend f graph1) (extend f graph2)
                     

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Implementați splitNode folosind extend!
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode node targets graph = extend (\n -> if n == node then (foldl (\x y -> (Overlay x (Node y))) Empty targets) else Node n) graph
                                

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul de tip AlgebraicGraph, astfel
    încât să puteți aplica o funcție pe toate etichetele unui graf.
    fmap reprezintă generalizarea lui map pentru orice fel de structură.

    Implementați fmap folosind extend!

    Exemplu:

    > fmap (+ 10) $ 1*(2+3) :: AlgebraicGraph Int
    (11*(12+13))
-}
instance Functor AlgebraicGraph where
    -- fmap :: (a -> b) -> AlgebraicGraph a -> AlgebraicGraph b
    fmap f graph = extend (\x -> Node (f x)) graph


{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Implementați mergeNodes folosind fmap!
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = fmap (\x -> if prop x == True then node else x) graph 

{-
    *** TODO ***

    Filtrează un graf, păstrând doar nodurile care satisfac proprietatea dată.

    Implementați filterGraph folosind extend!
    
    Exemplu:

    > nodes $ filterGraph odd $ 1*(2+3)
    fromList [1,3]

    > edges $ filterGraph odd $ 1*(2+3)
    fromList [(1,3)]
-}
filterGraph :: (a -> Bool) -> AlgebraicGraph a -> AlgebraicGraph a
filterGraph prop graph = extend (\x -> if prop x == True then Node x else Empty) graph

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Implementați removeNode folosind filterGraph!
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = filterGraph (\x -> x /= node) graph