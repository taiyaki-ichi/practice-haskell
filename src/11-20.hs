
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

