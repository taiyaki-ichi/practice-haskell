import Data.Char
import Data.List
import Control.Applicative

aaa::IO()
aaa= do
    line <- getLine
    let line' = reverse line
    putStrLn line'

bbb::IO()
bbb = do
    line <- fmap reverse getLine
    putStrLn line

ccc::IO()
ccc = do 
    str <- fmap (++"!") getLine
    putStrLn str 

ddd::IO()
ddd = do
    str <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn str 

eee::Num a=>[a]
eee = (+) <$> [1,2,3] <*> [1,2,3]

fff::IO()
fff = do
    str <- (++) <$> getLine <*> getLine
    putStrLn str

ggg::Num a => a
ggg = (+) <$> (+3) <*> (*2) $ 2

hhh::Num a=> [a]
hhh = (\x y z -> [x,y,z]) <$> (+3) <*> (+2) <*> const 0 $ 1

iii :: [Integer]
iii = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [1,2,3]

jjj :: [Integer]
jjj = getZipList $ (*) <$> ZipList [1,2,3] <*> ZipList [1,2,3]

sequenceA'::(Applicative f)=>[f a]->f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

sequenceA''::(Applicative f) => [f a] -> f [a]
sequenceA'' = foldr (\ x -> (<*>) ((:) <$> x)) (pure [])
