
--31
isPrime::Int->Bool
isPrime n = foldl (\ac x->if ((n `mod` x) /= 0) && ac then True else False) True [2..(n-1)]

--32
myGCD::Integer -> Integer -> Integer
myGCD a b 
    | b == 0 = abs a
    | otherwise = myGCD b (a `mod` b)

--33
comprime::Integer->Integer->Bool
comprime a b = gcd a b == 1

--34
totient::Integer->Int
totient n = length $ filter (\x -> gcd n x ==1) [1..n-1]

--35
{-
primeFactors::Integer->[Integer]
primeFactors = func []
    where 
        func lst 1 = lst
        func lst n = let h = minimum $ filter (\x -> gcd n x ==1) [2..n-1] 
            in h:(func lst (n/h))
            -}


