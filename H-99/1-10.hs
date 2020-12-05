
myLast::[a]->a
myLast = head.reverse

myButLast::[a]->a
myButLast list = x
    where
        (_:x:_)=reverse list

myButLast'::[a]->a
myButLast' xs=
    let (_:x:_)=reverse xs
    in x

myButLast''::[a]->a
myButLast'' xs = reverse xs !! 1

elementAt::[a]->Int->a
elementAt (x:xs) 1 = x
elementAt (x:xs) num =elementAt xs (num-1)

elementAt'::[a]->Int->a
elementAt' (x:_) 1 = x
elementAt' [] _ =error "index out of bounds"
elementAt' (_:xs) k
    | k<1 =error "index out of bounds"
    | otherwise =elementAt' xs (k-1)

myLength::[a]->Int
myLength = foldl (\ac _ ->ac+1) 0

myLength'::[a]->Int
myLength' [] = 0
myLength' (_:xs) = 1+myLength' xs

myReverse::[a]->[a]
myReverse =foldl (\ac x->x:ac) []

myReverse'::[a]->[a]
myReverse' [] =[]
myReverse' (x:xs) =myReverse' xs ++ [x]

isPalindrome::(Eq a)=>[a]->Bool
isPalindrome xs =xs == (reverse xs)

isPalindrome'::(Eq a)=>[a]->Bool
isPalindrome' = reverse >>= (==)

isPalindrome'''::(Eq a)=>[a]->Bool
isPalindrome''' = do
    r<-reverse
    (==) r

isPalindrome''''::(Eq a)=>[a]->Bool
isPalindrome'''' xs = let
    left=reverse xs
    right=xs
    in left==right

data NestedList a =Elem a |List [NestedList a]

flatten::NestedList a->[a]
flatten (Elem x)=[x]
flatten (List [])=[]
flatten (List (x:[]))=flatten x
flatten (List (x:xs))=(flatten x)++ (flatten (List xs))

flatten'::NestedList a->[a]
flatten' (Elem x) = [x]
flatten' (List (x:xs)) = flatten x ++flatten (List xs)
flatten' (List []) =[]


compress::(Eq a)=>[a]->[a]
compress = foldl check []
    where 
        check [] x =[x]
        check ac x=if last ac==x then ac else (ac++[x])

pack::(Eq a)=>[a]->[[a]]
pack = foldr func [[]]
    where
        func x [[]] = [[x]]
        func x (ac:acs) = if x==head ac
                then ([x]++ac):acs
                else [x]:ac:acs

    
encode::(Eq a)=>[a]->[(Int,a)]
encode =foldr func []
    where
        func x [] = [(1,x)]
        func x (ac:acs) = if snd ac == x
            then ((fst ac + 1),x):acs
            else (1,x):ac:acs
