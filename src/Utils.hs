{-# LANGUAGE TupleSections #-}
module Utils (
    uncurry3
, (.:)
, lookup'
, fst3
, snd3
, thd3
, first3
, second3
, third3) where


    

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)


lookup' :: Eq a => a -> [(a,b)] -> Maybe (a,b)
lookup' a ls = (a,) <$> lookup a ls


fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a
snd3 :: (a, b, c) -> b
snd3 (a,b,c) = b 
thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c
first3 :: (a -> d) -> (a,b,c) -> (d,b,c)
first3 f (a,b,c) = (f a,b,c)
second3 :: (b -> d) -> (a,b,c) -> (a,d,c)
second3 f (a,b,c) = (a,f b,c)
third3 :: (c -> d) -> (a,b,c) -> (a,b,d)
third3 f (a,b,c) = (a,b,f c)