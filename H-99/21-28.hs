
import System.Random

import Data.List

--21
insertAt::a->[a]->Int->[a]
insertAt x lst n = (take (n-1) lst)++[x]++(drop (n-1) lst)

--22
range::Int->Int->[Int]
range begin end = [begin..end]

--23
{-
rnd_select::[a]->Int->IO [a]
rnd_select lst 0 = return lst
rnd_select lst n = do
    rnd<-getStdGen
    rndNum<- randomR 0 (length lst)
    let the = lst !! rndNum
    let (left,right) = split the lst
    return rnd_select 
-}

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [xs !! x | x<-randomRs (0,(length xs)-1) gen]

--24
{-
diff_select::Int->Int->IO [Int]
diff_select n m = return $ func [1..m] n
    where
        func lst 0 = lst
        func lst n = do
            rnd <- getStdRandom $ randomR (0 ,(length lst))
            x <- lst !! rnd
            left <- take (rnd-1) lst
            right <- drop rnd lst
            a
-}

diff_select::Int->Int->IO [Int]
diff_select n m = func [1..m] n
    where
        func _ 0 = return []
        func lst n = do
            rnd <- randomRIO (0,(length lst -1))
            let left = take rnd lst
            let right = drop (rnd+1) lst
            rest <- func (left++right) (n-1)
            return ((lst!!rnd):rest)

--25
rnd_permu::[a]->IO [a]
rnd_permu lst = func lst []
    where
        func [] lst = return lst
        func l r = do
            rnd <- randomRIO (0,(length l -1))
            let x = l !! rnd
            let rest = (take rnd l)++(drop (rnd + 1) l)
            func rest (x:r) 

--26
combinations::Int->[a]->[[a]]
combinations 0 _ = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs , ys <- combinations (n-1) xs'] 

combinations'::Int->[a]->[[a]]
combinations' 0 _ = return []
combinations' n xs = do
    y:xs' <- tails xs
    ys <- combinations' (n-1) xs'
    return (y:ys)


--27,28は問題文が長いです。

    