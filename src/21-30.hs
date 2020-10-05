
import System.Random

--21
insertAt::a->[a]->Int->[a]
insertAt x lst n = (take (n-1) lst)++[x]++(drop (n-1) lst)

--22
range::Int->Int->[Int]
range begin end = [begin..end]

--23
rnd_select::[a]->Int->IO [a]
rnd_select lst 0 = return []
rnd_select lst n = do
    rnd<-getStdGen
    rndNum<- randomR 0 (length lst)
    let the = lst !! rndNum
    let (left,right) = split the lst
    return rnd_select 

    
    
    