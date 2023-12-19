-- | Traversable functions.
module Music.Theory.Traversable where

import Data.List {- base -}

{- | Replace elements at 'Traversable' with result of joining with elements from list.

> let t = Tree.Node 0 [Tree.Node 1 [Tree.Node 2 [],Tree.Node 3 []],Tree.Node 4 []]
> putStrLn $ Tree.drawTree (fmap show t)
> let (_,u) = adopt_shape (\_ x -> x) "abcde" t
> putStrLn $ Tree.drawTree (fmap return u)
-}
adopt_shape :: Traversable t => (a -> b -> c) -> [b] -> t a -> ([b], t c)
adopt_shape jn l =
  let f (i : j) k = (j, jn k i)
      f [] _ = error "adopt_shape: rhs ends"
  in mapAccumL f l

{- | Two-level variant of 'adopt_shape'.

>>> adopt_shape_2 (,) [0..4] (words "a bc d")
([4],[[('a',0)],[('b',1),('c',2)],[('d',3)]])
-}
adopt_shape_2 :: (Traversable t, Traversable u) => (a -> b -> c) -> [b] -> t (u a) -> ([b], t (u c))
adopt_shape_2 jn = mapAccumL (adopt_shape jn)

{- | Adopt stream to shape of traversable and zip elements.

>>> adopt_shape_2_zip_stream [1..] ["a", "list", "of", "strings"]
[[(1,'a')],[(2,'l'),(3,'i'),(4,'s'),(5,'t')],[(6,'o'),(7,'f')],[(8,'s'),(9,'t'),(10,'r'),(11,'i'),(12,'n'),(13,'g'),(14,'s')]]
-}
adopt_shape_2_zip_stream :: (Traversable t, Traversable u) => [c] -> t (u a) -> t (u (c, a))
adopt_shape_2_zip_stream s l = snd (adopt_shape_2 (flip (,)) s l)

{- | Two-level variant of 'zip' [1..]

>>> list_number_2 ["number","list","two"]
[[(1,'n'),(2,'u'),(3,'m'),(4,'b'),(5,'e'),(6,'r')],[(7,'l'),(8,'i'),(9,'s'),(10,'t')],[(11,'t'),(12,'w'),(13,'o')]]
-}
list_number_2 :: [[x]] -> [[(Int, x)]]
list_number_2 = adopt_shape_2_zip_stream [1 ..]

{- | Variant of 'adopt_shape' that considers only 'Just' elements at 'Traversable'.

> import Music.Theory.List
> let s = "a(b(cd)ef)ghi"
> let t = group_tree ((==) '(',(==) ')') s
> adopt_shape_m (,) [1..13] t
-}
adopt_shape_m :: Traversable t => (a -> b -> c) -> [b] -> t (Maybe a) -> ([b], t (Maybe c))
adopt_shape_m jn l =
  let f (i : j) k = case k of
        Nothing -> (i : j, Nothing)
        Just k' -> (j, Just (jn k' i))
      f [] _ = error "adopt_shape_m: rhs ends"
  in mapAccumL f l
