module ListSeq where
import Seq
import Par

empty :: [a]
empty = []

singleton :: a -> [a]
singleton x = [x]

length' :: [a] -> Int
length' xs = length xs

nth :: [a] -> Int -> a
nth [] _ = error "Lista vacia o indice invalido"
nth (x:xs) n | n == 0 = x
             | otherwise = nth xs (n - 1)

tabulate :: (Int -> a) -> Int -> [a]
tabulate f n | n < 0 = error "Argumento inválido"
             | otherwise = tabulate_aux f 0 n
                            where 
                              tabulate_aux f i m | i == m = []
                                                 | otherwise = let (fx, rest) = (f i) ||| (tabulate_aux f (i + 1) m)
                                                               in fx : rest

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = let (fx, rest) = f x ||| map' f xs
                in fx : rest

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = let (px, rest) = p x ||| filter' p xs
                   in if px then x : rest else rest

append :: [a] -> [a] -> [a]
append [] ys = ys
append xs [] = xs
append (x:xs) ys = x : (append xs ys)

take' :: [a] -> Int -> [a]
take' xs n = take n xs

drop' :: [a] -> Int -> [a]
drop' xs n = drop n xs

showt :: [a] -> TreeView a [a]
showt [] = EMPTY
showt [x] = ELT x
showt xs = let len = length' xs
               s = div len 2
               (t1, t2) = take' xs s ||| drop' xs s
           in NODE t1 t2

showl :: [a] -> ListView a [a]
showl [] = NIL
showl (x:xs) = CONS x xs

join :: [[a]] -> [a]
join [] = []
join (xs:xss) = append xs (join xss)

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce f e [] = e
reduce f e [x] = f e x
reduce f e ls = let red = reduce_aux ls
                in reduce_aux2 red
                  where
                    reduce_aux [] = []
                    reduce_aux [x] = [x] 
                    reduce_aux (x:y:xs) = let (op, reduc) = (f x y) ||| (reduce_aux xs)
                                          in op : reduc
                    reduce_aux2 [x] = f e x
                    reduce_aux2 xs = reduce f e xs


scan :: (a -> a -> a) -> a -> [a] -> ([a], a)
scan f e s = let (ys, total) = scanAux f e s
                 sec = completar f s ys 0
             in (sec, total)
              where
                completar f [] ys _ = []
                completar f xs [] _ = []
                completar f [x] (y:ys) i = case even i of
                                                True -> y : (completar f [x] (y:ys) (i + 1))
                                                False -> []
                completar f (x:x':xs) (y:ys) i = case even i of
                                                True -> y : (completar f (x:x':xs) (y:ys) (i + 1))
                                                False -> let (op, rest) = (f y x) ||| (completar f xs ys (i + 1))
                                                         in op : rest

scanAux :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanAux f e s = let sec = (reduccion f (e: e:(contraccion f s)))
                    len = length' sec
                    (secFinal, total) = take' sec (len - 1) ||| nth sec (len - 1)
                in (secFinal, total)
            
contraccion :: (a -> a -> a) -> [a] -> [a]            
contraccion f [] = []
contraccion f [x] = [x]
contraccion f (x:y:xs) = let (res, contra) = (f x y) ||| (contraccion f xs)
                         in res : contra

reduccion :: (a -> a -> a) -> [a] -> [a]
reduccion f [] = []
reduccion f [x] = [x]
reduccion f (x:y:xs) = res : (reduccion f (res:xs))  
      where res = (f x y)


instance Seq [] where
  emptyS = empty
  singletonS = singleton
  lengthS = length
  nthS = nth
  tabulateS = tabulate
  mapS = map'
  filterS = filter'
  appendS = append
  takeS = take'
  dropS = drop'
  showtS = showt
  showlS = showl
  joinS = join
  reduceS = reduce
  scanS = scan
  fromList = id