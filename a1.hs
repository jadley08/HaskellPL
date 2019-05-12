-- http://cgi.soic.indiana.edu/~c311/doku.php?id=resources

-- 0: PREREQUISITES ---------------------------------------------
sub1 :: (Integral n) => n -> n
sub1 0 = 0
sub1 n = n - 1

add1 :: (Integral n) => n -> n
add1 n = n + 1

add1maybe :: (Maybe Int) -> (Maybe Int)
add1maybe Nothing = Nothing
add1maybe (Just n) = Just (add1 n)

myeven :: Int -> Bool
myeven 0 = True
myeven n = not (myeven (sub1 n))

-- 1: COUNTDOWN ---------------------------------------------
countdown :: (Integral n) => n -> [n]
countdown 0 = [0]
countdown n = n : (countdown (n - 1))

-- 2: COUNTDOWN ---------------------------------------------
insertR :: (Eq v) => v -> v -> [v] -> [v]
insertR _ _ [] = []
insertR x y (a:d) = if x == a
                    then x : y : (insertR x y d)
                    else a : (insertR x y d)

-- 3: REMVFIRST ---------------------------------------------
remvfirst :: (Eq v) => v -> [v] -> [v]
remvfirst _ [] = []
remvfirst x (a:d) = if x == a
                    then d
                    else a : (remvfirst x d)

-- 4: LISTINDEXOFV ---------------------------------------------
listindexofv :: (Eq v) => v -> [v] -> Int
listindexofv _ [] = 0
listindexofv x (a:d) = if x == a
                       then 0
                       else add1 (listindexofv x d)

-- 4 1/2: LISTINDEXOFVMAYBE ---------------------------------------------
listindexofvmaybe :: (Eq v) => v -> [v] -> (Maybe Int)
listindexofvmaybe _ [] = Nothing
listindexofvmaybe x (a:d) = if x == a
                            then Just 0
                            else add1maybe (listindexofvmaybe x d)

-- 5: LISTINDEXOFVMAYBE ---------------------------------------------
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter p (a:d) = if (p a)
                   then a : (myfilter p d)
                   else (myfilter p d)

-- 6: ZIP ---------------------------------------------
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (a1:d1) (a2:d2) = (a1,a2):(myzip d1 d2)

-- 7: MAP ---------------------------------------------
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap p (a:d) = (p a):(mymap p d)

-- 8: APPEND ---------------------------------------------
myappend :: [a] -> [a] -> [a]
myappend [] ls2 = ls2
myappend (a:d) ls2 = a:(myappend d ls2)

-- 9: REVERSE ---------------------------------------------
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (a:d) = (myappend (myreverse d) [a])

-- 10: FACT ---------------------------------------------
fact :: Int -> Int
fact 0 = 1
fact n = n * (fact (sub1 n))

-- 11: MEMV ---------------------------------------------
memv :: (Eq v) => v -> [v] -> (Maybe [v])
memv _ [] = Nothing
memv x (a:d) = if x == a
               then Just (a:d)
               else memv x d

-- 12: FIB ---------------------------------------------
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (sub1 n) + fib (sub1 (sub1 n))

-- 14: BINARYNATURAL ---------------------------------------------
data Bin = One | Zero

binarynatural :: [Bin] -> Int
binarynatural [] = 0
binarynatural (Zero:d) = 2 * (binarynatural d)
binarynatural (One:d) = 1 + 2 * (binarynatural d)

-- 15: MINUS ---------------------------------------------
minus :: Int -> Int -> Int
minus n 0 = n
minus n m = minus (sub1 n) (sub1 m)

-- 16: DIV ---------------------------------------------
mydiv :: Int -> Int -> Int
mydiv 0 m = 0
mydiv n m = add1 (mydiv (n - m) m)

-- 17: APPEND-MAP ---------------------------------------------
appendmap :: (a -> [b]) -> [a] -> [b]
appendmap _ [] = []
appendmap p (a:d) = myappend (p a) (appendmap p d)

-- 18: SET-DIFFERENCE ---------------------------------------------
setdifference :: (Eq a) => [a] -> [a] -> [a]
setdifference [] _ = []
setdifference (a:d) s2 = if (memv a s2) == Nothing
                         then a : (setdifference d s2)
                         else (setdifference d s2)

-- TESTS ---------------------------------------------
main =
  print [ ((countdown 5) == [5,4,3,2,1,0]),
          ((insertR 1 2 [1,3,3,1,2,1]) == [1,2,3,3,1,2,2,1,2]),
          ((remvfirst 1 [1,2,3,1]) == [2,3,1]),
          ((remvfirst 2 [1,2,3,2,1]) == [1,3,2,1]),
          ((listindexofv 1 [1,2,3,1,1]) == 0),
          ((listindexofv 1 [2,3,1,1]) == 2),
          ((listindexofv 0 [2,3,1,1]) == 4),
          ((listindexofvmaybe 1 [1,2,3,1,1]) == Just 0),
          ((listindexofvmaybe 1 [2,3,1,1]) == Just 2),
          ((listindexofvmaybe 0 [2,3,1,1]) == Nothing),
          ((myfilter even [1,2,3,4,5,6]) == [2,4,6]),
          ((zip [1,2,3] [True,False,True]) == [(1,True),(2,False),(3,True)]),
          ((zip [1,2,3,4,5,6] [True,False,True]) == [(1,True),(2,False),(3,True)]),
          ((zip [1,2,3] [True,False,True,False,True,False]) == [(1,True),(2,False),(3,True)]),
          ((mymap add1 [1,2,3,4]) == [2,3,4,5]),
          ((myappend [1,2,3] [4,5,6]) == [1,2,3,4,5,6]),
          ((myreverse [1,2,3]) == [3,2,1]),
          ((fact 0) == 1),
          ((fact 5) == 120),
          ((memv 1 [1,2,3]) == Just [1,2,3]),
          ((memv 2 [1,0,3]) == Nothing),
          ((memv 2 [1,2,3,2]) == Just [2,3,2]),
          ((fib 0) == 0),
          ((fib 1) == 1),
          ((fib 7) == 13),
          ((binarynatural []) == 0),
          ((binarynatural [Zero,Zero,One]) == 4),
          ((binarynatural [Zero,Zero,One,One]) == 12),
          ((binarynatural [One,One,One,One]) == 15),
          ((binarynatural [One,Zero,One,Zero,One]) == 21),
          ((binarynatural [One,One,One,One,One,One,One,One,One,One,One,One,One]) == 8191),
          ((minus 5 3) == 2),
          ((minus 100 50) == 50),
          ((mydiv 25 5) == 5),
          ((mydiv 36 6) == 6),
          ((appendmap countdown (countdown 5)) == [5,4,3,2,1,0,4,3,2,1,0,3,2,1,0,2,1,0,1,0,0]),
					((setdifference [1,2,3,4,5] [2,4,6,8]) == [1,3,5])
        ]
