module ArrSeq where
import Prelude hiding (length, map, filter, take, drop)
import qualified Arr as A
import Arr (Arr, (!), length, subArray, tabulate, empty, flatten)

import Seq
import Par

singleton :: a -> Arr a
singleton x = A.fromList [x]

nth :: Arr a -> Int -> a
nth xs i = xs ! i

map :: (a -> b) -> Arr a -> Arr b
map f xs = tabulate (\i -> f (xs ! i)) (length xs)

filter :: (a -> Bool) -> Arr a -> Arr a
filter p xs = flatten (map (\x -> if p x then singleton x else empty) xs)

append :: Arr a -> Arr a -> Arr a
append xs ys = let (lenxs, lenys) = length xs ||| length ys
                   n = lenxs + lenys
               in tabulate (\i -> if i < lenxs then xs ! i else ys ! (i - lenxs)) n

take :: Arr a -> Int -> Arr a
take xs n = subArray 0 n xs

drop :: Arr a -> Int -> Arr a
drop xs n = subArray n (length xs - n) xs

showt :: Arr a -> TreeView a (Arr a)
showt xs = case length xs of
            0 -> EMPTY
            1 -> ELT (xs ! 0)
            n -> NODE t d where tam = div n 2
                                (t, d) = take xs tam ||| drop xs tam

showl :: Arr a -> ListView a (Arr a)
showl xs = case length xs of
            0 -> NIL
            _ -> CONS (xs ! 0) (drop xs 1)

showt' :: Arr a -> TreeView a (Arr a)
showt' xs = case length xs of
            0 -> EMPTY
            1 -> ELT (xs ! 0)
            n -> NODE t d where tam = 2 ^ ((floor . logBase 2) ((fromIntegral n)-1))
                                (t, d) = take xs tam ||| drop xs tam

reduce :: (a -> a -> a) -> a -> Arr a -> a
reduce f e ls = case showt' ls of 
                  EMPTY -> e
                  _ -> f e (reduce_aux ls)
                        where
                            reduce_aux s = case showt' s of
                                              ELT x -> x
                                              NODE l r -> let (l', r') = reduce_aux l ||| reduce_aux r
                                                          in f l' r'


scan :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
scan f e s = let (ys, total) = scanAux f e s
                 sec = completar f s ys 0
             in (sec, total)
              where
                completar f xs ys i | length xs == 0 || length ys == 0 = empty
                                    | otherwise = case even i of
                                                    True -> append (singleton (ys ! 0)) (completar f xs ys (i + 1))
                                                    False -> if length xs == 1 then empty else append (singleton (f (ys ! 0) (xs ! 0))) (completar f (drop xs 2) (drop ys 1) (i + 1))

scanAux :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
scanAux f e s = let e' = singleton e
                    sec = (reduccion f (append e'(append e' (contraccion f s))))
                    len = length sec
                    (secFinal, total) = take sec (len - 1) ||| nth sec (len - 1)
                in (secFinal, total)

contraccion :: (a -> a -> a) -> Arr a -> Arr a            
contraccion f ls = case length ls of
                    0 -> empty
                    1 -> ls
                    _ -> let (x, y) = ls ! 0 ||| ls ! 1
                             (xs, op) = drop ls 2 ||| f x y
                             (sing, ys) = singleton op ||| contraccion f xs
                         in append sing ys

reduccion :: (a -> a -> a) -> Arr a -> Arr a
reduccion f ls = case length ls of
                    0 -> empty
                    1 -> ls
                    _ -> let (x, y) = ls ! 0 ||| ls ! 1
                             xs = drop ls 2
                             sing = singleton (f x y)
                             ys = append sing xs
                             red = reduccion f ys
                         in append sing red


instance Seq Arr where
  emptyS = empty
  singletonS = singleton
  lengthS = length
  nthS = nth
  tabulateS = tabulate
  mapS = map
  filterS = filter
  appendS = append
  takeS    = take
  dropS    = drop
  showtS   = showt
  showlS   = showl
  joinS    = flatten
  reduceS  = reduce
  scanS    = scan
  fromList = A.fromList






