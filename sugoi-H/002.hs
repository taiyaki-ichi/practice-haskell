
import Control.Applicative
import Control.Monad

aaa::IO()
aaa = do 
    line <- getLine
    let line' = reverse line
    putStrLn line' 

bbb::IO()
bbb = do 
    line <- fmap reverse getLine
    putStrLn line 

ccc::IO String
ccc = (++) <$> getLine <*> getLine

ddd::[Integer]
ddd = liftA2 (\x y -> x+y) [1,2,3] [2,3,4]



type Bird = Int 
type Pole = (Bird,Bird) 

landLeft::Bird -> Pole -> Pole
landLeft n (l,r) = (l+n,r) 

landRight::Bird -> Pole -> Pole 
landRight n (l,r) = (l,r+n) 

(-:)::a -> (a -> b) -> b
(-:) x f = f x

test001::Pole
test001 = (0,0) -: landLeft 2 -: landRight 3

landLeft'::Bird -> Pole -> Maybe Pole 
landLeft' n (left,right) 
    | abs ((left+n)-right) < 4 = Just (left + n,right)
    | otherwise = Nothing

landRight'::Bird -> Pole -> Maybe Pole 
landRight' n (left,right) 
    | abs (left-(right+n)) < 4 = Just (left,right+n) 
    | otherwise = Nothing

test002::Maybe Pole
test002 = return (0,0) >>= landLeft' 3 >>= landRight' 4 >>= landRight' 1 

test003::Maybe Pole 
test003 = return (0,0) >>= landLeft' 2 >>= landRight' 10 >>= landLeft' 1

banana::Pole -> Maybe Pole 
banana _ = Nothing

test004::Maybe Pole 
test004 = return (0,0) >>= landLeft' 2 >>= banana >>= landRight' 2

test005::Maybe Pole 
test005 = return (0,0) >>= landLeft' 4 >> Nothing >>= landRight' 2

test006::Maybe String 
test006 = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

test007::Maybe String 
test007 = do 
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

test008:: Maybe Pole 
test008 = do 
    start <- return (0,0)
    a <- landLeft' 2 start 
    b <- landRight' 1 a 
    landRight' 1 b


test009:: Maybe Pole 
test009 = do 
    start <- return (0,0)
    a <- landLeft' 2 start 
    Nothing 
    b <- landRight' 1 a 
    landRight' 1 b


test010::[(Int,Char)]
test010 = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch) 

test011::[(Int,Char)]
test011 = do 
    n <- [1,2]
    ch <- ['a','b']
    return  (n,ch)

test012::[(Int,Char)]
test012 = [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]

test013::[Int]
test013 = [1..10] >>= (\n -> guard (odd n) >> return n)

test014::[Int]
test014 = do 
    n <- [1..10]
    guard (odd n)
    return n

