module Data.Queue (
    Queue(),
    empty,
    singleton,
    toList,
    fromList,
    enqueue,
    dequeue
) where

data Queue a = Queue [a] [a] -- Queue inlist uitlist

empty :: Queue a
empty = Queue [] []

singleton :: a -> Queue a
singleton a = Queue [] [a]

fromList :: [a] -> Queue a
fromList = Queue []

toList :: Queue a -> [a]
toList (Queue i o) = o ++ (reverse i)

enqueue :: Queue a -> a -> Queue a
enqueue (Queue i o) a = Queue (a:i) o

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [] []) = error "dequeing empty queue"
dequeue (Queue i []) = let (x:xs) = reverse i in (x, Queue [] xs)
dequeue (Queue i (o:os)) = (o, Queue i os)