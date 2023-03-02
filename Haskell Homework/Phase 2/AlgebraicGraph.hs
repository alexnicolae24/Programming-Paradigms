module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

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

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = case graph of
                           Empty -> Empty 
                           (Node a) -> if node == a then Empty else Node a
                           (Overlay graph1 graph2) -> Overlay (rN graph1) (rN graph2)
                           (Connect graph1 graph2) -> Connect (rN graph1) (rN graph2)
                        where 
                            rN = removeNode node


{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news graph = case graph of
                            Empty -> Empty 
                            (Node a) -> if old == a then (foldl (\x y -> (Overlay x (Node y))) Empty news) else Node a
                            (Overlay graph1 graph2) -> Overlay (sN graph1) (sN graph2)
                            (Connect graph1 graph2) -> Connect (sN graph1) (sN graph2)
                        where 
                            sN = splitNode old news




{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = case graph of
                               Empty -> Empty 
                               (Node a) -> if (prop a == False) then Overlay Empty (Node a) else Overlay Empty (Node node)
                               (Overlay graph1 graph2) -> Overlay (mN graph1) (mN graph2)
                               (Connect graph1 graph2) -> Connect (mN graph1) (mN graph2)
                        where 
                            mN = mergeNodes prop node
