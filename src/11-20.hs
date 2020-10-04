
import Data.List as List

data CntItem a = Multiple Int a | Single a deriving(Show)

--11
encodeModified::(Eq a)=>[a]->[CntItem a]
encodeModified = foldr func []
    where 
        func x [] = [Single x]
        func x ((Single ac):acs) = if ac == x
            then (Multiple 2 x):acs
            else (Single x):(Single ac):acs
        func x ((Multiple n ac):acs) = if ac == x
            then (Multiple (n+1) ac):acs
            else (Single x):(Multiple n ac):acs

encodeModified'::(Eq a)=>[a]->[CntItem a]
encodeModified' xs = [y | x <- List.group xs ,
    let y = if (length x) == 1
        then Single (head x) 
        else Multiple (length x) (head x)
        ]
 --12
decodeModified::(Eq a)=>[CntItem a]->[a]
decodeModified = foldr func []
    where 
        func (Single x) ac = x:ac 
        func (Multiple n x) ac = (replicate n x)++ac

--14
dupli::[a]->[a]
dupli = foldr (\x ac->x:x:ac) []

--15
repli::[a]->Int->[a]
repli list num = foldr (\x ac->(replicate num x)++ac) [] list

--16
dropEvery::[a]->Int->[a]
dropEvery [] _ = []
dropEvery list num = (take (num-1) list)++dropEvery (drop num list) num

--17
split::[a]->Int->([a],[a])
split list num = ((take num list),(drop num list))

split'::[a]->Int->([a],[a])
split' list num = foldr func ([],[]) list
    where
        leftLength=length list - num
        func x ([],[]) = ([],[x])
        func x ([],left) = if length left >= leftLength
            then ([x],left) else ([],x:left)
        func x (right,left) = (x:right,left)

--18
slice::[a]->Int->Int->[a]
slice lst begin end = take (end-begin+1) $ drop (begin-1) lst

--19
rotate::[a]->Int->[a]
rotate lst 0 = lst
rotate lst n
    | n>0 = rotate ((tail lst)++[head lst]) (n-1)
    | n<0 = rotate ([last lst]++(init lst)) (n+1)

--20
removeAt::Int->[a]->(a,[a])
removeAt n lst = (lst!!(n-1),(take (n-1) lst)++(drop n lst))