module Helpers where

--data Exp = Var String | Lam String Exp | App Exp Exp | EInt Int deriving (Show)

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

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter p (a:d) = if (p a)
                   then a : (myfilter p d)
                   else (myfilter p d)

myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (a1:d1) (a2:d2) = (a1,a2):(myzip d1 d2)

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap p (a:d) = (p a):(mymap p d)

myappend :: [a] -> [a] -> [a]
myappend [] ls2 = ls2
myappend (a:d) ls2 = a:(myappend d ls2)

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (a:d) = (myappend (myreverse d) [a])

memv :: (Eq v) => v -> [v] -> (Maybe [v])
memv _ [] = Nothing
memv x (a:d) = if x == a
               then Just (a:d)
               else memv x d

member :: (Eq v) => v -> [v] -> Bool
member _ [] = False
member x (a:d) = if x == a
                 then True
								 else member x d

assv :: (Eq a) => a -> [(a,b)] -> Maybe (a,b)
assv _ [] = Nothing
assv v ((x,y):d) = if v == x
                   then Just (x,y)
									 else assv v d
