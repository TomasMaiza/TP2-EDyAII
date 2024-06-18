module ArrSeq where
import Prelude hiding (length)
import qualified Arr as A
import Arr (Arr, (!), length, subArray, tabulate, empty, flatten)

import Seq
import Par

--empty':: Arr a 
--empty' = empty

singleton :: a -> Arr a
singleton x = A.fromList [x]

length' :: Arr a -> Int
length' xs = length xs 

nth :: Arr a -> Int -> a
nth xs i = xs ! i

tabulate' :: (Int -> a) -> Int -> Arr a
tabulate' f n = tabulate f n

map' :: (a -> b) -> Arr a -> Arr b
map' f xs = tabulate (\i -> f (xs ! i)) (length xs)

filter' :: (a -> Bool) -> Arr a -> Arr a
filter' p xs = flatten (map' (\x -> if p x then singleton x else empty) xs)

append :: Arr a -> Arr a -> Arr a
append xs ys = let (lenxs, lenys) = length xs ||| length ys
                   n = lenxs + lenys
               in tabulate (\i -> if i < lenxs then xs ! i else ys ! (i - lenxs)) n

take' :: Arr a -> Int -> Arr a
take' xs n = subArray 0 n xs

drop' :: Arr a -> Int -> Arr a
drop' xs n = subArray n (length xs - n) xs

showt :: Arr a -> TreeView a (Arr a)
showt xs = case length xs of
            0 -> EMPTY
            1 -> ELT (xs ! 0)
            n -> NODE (take' xs tam) (drop' xs tam) where tam = div n 2

showl :: Arr a -> ListView a (Arr a)
showl xs = case length xs of
            0 -> NIL
            _ -> CONS (xs ! 0) (drop' xs 1)

reduce :: (a -> a -> a) -> a -> Arr a -> a
reduce f e ls = case showl ls of
                    NIL -> e
                    CONS y empty -> f e y
                    CONS y ys -> reduce_aux (CONS y ys)
                                    where
                                        reduce_aux :: ListView a (Arr a) -> a 
                                        reduce_aux (CONS x empty) = x
                                        reduce_aux (CONS x xs) = let (result, rest) = f x ||| reduce_aux xs
                                                                 in reduce_aux (CONS result rest) 


instance Seq Arr where
  emptyS = empty
  singletonS = singleton
  lengthS = length
  nthS = nth
  tabulateS = tabulate
  mapS = map'
  filterS = filter'
  appendS = append
  takeS    = take'
  dropS    = drop'
  showtS   = showt
  showlS   = showl
  joinS    = flatten
  reduceS  = reduce
  scanS    = undefined
  fromList = undefined






