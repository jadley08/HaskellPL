import Helpers
import Data.Maybe

-- Ackermann & Fibonacci
emptyk :: v -> v
emptyk v = v

emptys = []

-- Normal -------------------
ack :: Int -> Int -> Int
ack 0 n = add1 n
ack m 0 = ack (sub1 m) 1
ack m n = ack (sub1 m) (ack m (sub1 n))

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib (sub1 n)) + (fib (sub1 (sub1 n)))

-- Continuation Passing Style -------------------
ackcps :: Int -> Int -> (Int -> Int) -> Int
ackcps 0 n k = k (add1 n)
ackcps m 0 k = ackcps (sub1 m) 1 k
ackcps m n k = ackcps m (sub1 n) (\almost -> ackcps (sub1 m) almost k)

-- Store Passing Style -------------------
acksps :: Int -> Int -> [((Int,Int),Int)] -> (Int,[((Int,Int),Int)])
acksps 0 n store = (add1 n,store)
acksps m n store = let lookup = assv (m,n) store in
                   if isJust lookup
                   then (snd (fromJust lookup),store)
                   else
                     if n == 0
                     then
                       let res = acksps (sub1 m) 1 store in
                       let resv = fst res in
                       let ress = snd res in
                       (resv,((m,n),resv) : ress)
                     else
                       let res1 = acksps m (sub1 n) store in
                       let res1v = fst res1 in
                       let res1s = snd res1 in
                       let res2 = acksps (sub1 m) res1v res1s in
                       let res2v = fst res2 in
                       let res2s = snd res2 in
                       (res2v, ((m,n),res2v) : res2s)

fibsps :: Int -> [(Int,Int)] -> (Int,[(Int,Int)])
fibsps 0 store = (0,store)
fibsps 1 store = (1,store)
fibsps n store = let lookup = assv n store in
                     if isJust lookup
                     then (snd (fromJust lookup),store)
                     else
                       let fibsub1 = fibsps (sub1 n) store in
                       let fibsub1v = fst fibsub1 in
                       let fibsub1s = snd fibsub1 in
                       let fibsub2 = fibsps (sub1 (sub1 n)) fibsub1s in
                       let fibsub2v = fst fibsub2 in
                       let fibsub2s = snd fibsub2 in
                       (fibsub1v + fibsub2v, (n,fibsub1v + fibsub2v) : fibsub2s)

-- Tests -----------------------------
main =
  print (
    ack 2 1 == 5
     &&
    ack 2 2 == 7
     &&
    fib 7 == 13
     &&
    ackcps 2 1 emptyk == 5
     &&
    ackcps 2 2 emptyk == 7
     &&
    acksps 2 1 emptys == (5,[((2,1),5),((1,3),5),((1,2),4),((2,0),3),((1,1),3),((1,0),2)])
     &&
    acksps 2 2 emptys == (7,[((2,2),7),((1,5),7),((1,4),6),((2,1),5),((1,3),5),((1,2),4),((2,0),3),((1,1),3),((1,0),2)])
     &&
    fibsps 7 emptys == (13,[(7,13),(6,8),(5,5),(4,3),(3,2),(2,1)])
  )
