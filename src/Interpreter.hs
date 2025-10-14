{-# LANGUAGE InstanceSigs #-}
module Interpreter (

runProgram) where

import qualified Data.DirGraph as G
import Instructions
import CodelType
import Utils
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isSpace, ord, chr)
import Text.Read (readMaybe)
import System.IO (isEOF)
import Data.Tuple (swap)
import Control.Applicative (asum)


data Piet a = Piet { 
    runPiet :: G.Graph [PInstruction] (DP,CC) -> G.Vertex -> CC -> DP -> [Int] -> IO (a, G.Vertex, CC, DP, [Int])
}

instance Functor Piet where
  fmap :: (a -> b) -> Piet a -> Piet b
  fmap f pa = Piet $ \g v c d s -> do
    (a, v',c',d',s') <- runPiet pa g v c d s
    pure (f a, v', c', d', s')

instance Applicative Piet where
  pure :: a -> Piet a
  pure a = Piet $ \_ v c d s -> pure (a, v, c, d, s)
  (<*>) :: Piet (a -> b) -> Piet a -> Piet b
  (Piet pa) <*> (Piet pb) = Piet $ \g v c d s -> do
    (a, v', c', d', s') <- pa g v c d s
    (b, v'', c'', d'', s'') <- pb g v' c' d' s'
    pure (a b, v'', c'', d'', s'')

instance Monad Piet where
  (>>=) :: Piet a -> (a -> Piet b) -> Piet b
  (Piet pa) >>= f = Piet $ \g v c d s -> do
    (a, v', c', d', s') <- pa g v c d s
    let (Piet pb) = f a
    (b, v'', c'', d'', s'') <- pb g v' c' d' s'
    return (b, v'', c'', d'', s'')

instance MonadIO Piet where
  liftIO :: IO a -> Piet a
  liftIO f = Piet $ \g v c d s -> f >>= \x -> pure (x,v,c,d,s)


rotateDPClockwise :: Int -> DP -> DP
rotateDPClockwise 0 d = d
rotateDPClockwise 1 N = E
rotateDPClockwise 1 E = S
rotateDPClockwise 1 S = W
rotateDPClockwise 1 W = N
rotateDPClockwise n d
    | n > 1 && n < 4 = rotateDPClockwise 1 $ rotateDPClockwise (n-1) d
    | otherwise = rotateDPClockwise (n `mod` 4) d 
-- >>> map (`mod` 4) [(-3)..3]
-- [1,2,3,0,1,2,3]

toggleCCn :: Int -> CC -> CC
toggleCCn 0 c = c
toggleCCn 1 CCRight = CCLeft
toggleCCn 1 CCLeft = CCRight
toggleCCn n c = toggleCCn (n `mod` 2) c

pointerOp :: Int -> Piet ()
pointerOp n = Piet $ \_ v c d s -> pure ((), v, c, rotateDPClockwise n d, s)

switchOp :: Int -> Piet ()
switchOp n = Piet $ \_ v c d s -> pure ((), v, toggleCCn n c, d, s)

getStack :: Piet [Int] 
getStack = Piet $ \_ v c d s -> pure (s, v, c, d, s)

getStackLength :: Piet Int
getStackLength = length <$> getStack

push :: Int -> Piet ()
push n = Piet $ \_ v c d s -> pure ((), v,c,d,n:s)

setStack :: [Int] -> Piet ()
setStack s = Piet $ \_ v c d _ -> pure ((), v,c,d,s)


pop :: Piet (Maybe Int)
pop = getStack >>= \s -> case s of 
    [] -> pure Nothing
    (x:xs) -> setStack xs *> pure (Just x)

pop2 :: Piet (Maybe (Int, Int))
pop2 = getStack >>= \s -> case s of 
    [] -> pure Nothing
    [_] -> pure Nothing
    (x:y:xs) -> setStack xs *> pure (Just (x,y))

binOpAlsKan :: (Int -> Int -> Piet ()) -> Piet ()
binOpAlsKan f = pop2 >>= \r -> case r of
    Nothing -> pure ()
    Just (x,y) -> f x y

unitOpAlsKan :: (Int -> Piet ()) -> Piet ()
unitOpAlsKan f = pop >>= \r -> case r of
    Nothing -> pure ()
    Just x -> f x

metStackRestore :: (Piet () -> Piet ()) -> Piet ()
metStackRestore f = do
    s <- getStack
    let restore = setStack s
    f restore
    
-- roll remaining stack to [tweedetop] depth [top] keer
-- A single roll to depth n is defined as burying the top value on the stack n deep and bringing all values above it up by 1 place. 
-- A negative number of rolls rolls in the opposite direction. 
-- A negative depth is an error and the command is ignored. 
-- If a roll is greater than an implementation-dependent maximum stack depth, it is handled as an implementation-dependent error, 
-- though simply ignoring the command is recommended.
rollOp :: Piet () -> Int -> Int -> Piet ()
rollOp restore top tweedetop = do
    stack <- getStack
    let depth = tweedetop
        numrolls = top
        stacksize = length stack
        numrolls' = numrolls `mod` depth
    case () of
        ()  | stacksize < depth -> restore
            | depth < 0 -> restore
            | depth <= 1 -> pure ()
            | numrolls' == 0 -> pure ()
            | otherwise -> 
                let eerstestuk = take numrolls' stack
                    tweedestuk = take (depth - numrolls' ) $ drop numrolls' stack
                    rest = drop depth stack
                    stack' = tweedestuk ++ eerstestuk ++ rest
                in setStack stack'

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

getNumInput :: Piet (Maybe Int)
getNumInput = do
    isClosed <- liftIO isEOF
    if isClosed 
        then pure Nothing
        else do
            s <- liftIO getLine
            case filter (not . isSpace) s of
                "" -> getNumInput -- een lege regel of regel met alleen spaties, dus opnieuw regel lezen
                ss -> pure $ readMaybe ss

pushNumInput :: Piet ()
pushNumInput = getNumInput >>= \x -> case x of
    Nothing -> pure ()
    Just n -> push n

getCharInput :: Piet ()
getCharInput = do
    isClosed <- liftIO isEOF
    if isClosed 
        then pure ()
        else (ord <$> liftIO getChar) >>= push


doeInstructions :: [PInstruction] -> Piet Bool
doeInstructions [] = pure True
doeInstructions (x:xs) = 
    let r = case x of
            PPush n -> push n
            PPop -> pop *> pure ()
            PAdd -> binOpAlsKan (push .: (+)) -- top + tweedetop
            PSubstract -> binOpAlsKan (push .: subtract) -- tweedetop - top
            PMultiply -> binOpAlsKan (push .: (+)) -- top * tweedetop
            PDivide -> metStackRestore $ \restore -> binOpAlsKan $ \top tweedetop ->  -- tweedetop / top
                if top == 0 
                    then restore
                    else push (tweedetop `quot` top) -- quot is integer division truncate naar 0
            PMod -> metStackRestore $ \restore -> binOpAlsKan $ \top tweedetop ->  -- tweedetop `mod` top
                if top == 0 
                    then restore
                    else push (tweedetop `mod` top) -- dit is mod, komt overeen met specificatie
            PNot -> unitOpAlsKan (push . boolToInt . (== 0))
            PGreater -> binOpAlsKan (push . boolToInt .: (<) ) -- tweedetop > top dus top < tweedetop
            PPointer -> unitOpAlsKan pointerOp
            PSwitch -> unitOpAlsKan switchOp
            PDuplicate -> unitOpAlsKan (\v -> push v *> push v)
            PRoll -> metStackRestore $ \restore -> binOpAlsKan $ rollOp restore
            -- we volgen https://github.com/your-diary/piet_programming_language als input
            PInNum -> pushNumInput
            POutNum -> unitOpAlsKan $ liftIO . putStr . show
            PInChar -> getCharInput
            POutChar -> unitOpAlsKan $ liftIO . putChar . chr
            PNop -> pure ()
            PStop -> pure () -- heeft zo losse uitzondering
            PSetDP d -> Piet $ \_ v c _ s -> pure ((), v, c, d, s)
            PSetCC c -> Piet $ \_ v _ d s -> pure ((), v, c, d, s)

    in 
        --(liftIO $ putStrLn $ "doeInstruction: " ++ show x) *> 
        r *> if x == PStop
        then pure False
        else doeInstructions xs
    
getInstructions :: Piet [PInstruction]
getInstructions = Piet $ \g v c d s -> pure (G.getVal v g, v, c, d, s)

runProgramLoop :: Piet ()
runProgramLoop = do
    --(v,c,d,s) <- Piet $ \g v c d s -> pure ((v,c,d,s), v, c, d, s)
    --liftIO $ putStr $ "we zijn in " ++ show v ++ " met richting " ++ show c ++ "," ++ show d ++ " en stack " ++ show s ++ "\n"
    ins <- getInstructions
    --liftIO $ putStr "Instructions: "
    --liftIO $ print ins
    doorgaan <- doeInstructions ins
    if not doorgaan
        then pure ()
        else doeStap >> runProgramLoop


doeStap :: Piet ()
doeStap = Piet $ \g v c d s -> do 
    let edges = map swap $ G.getArcs v g
        dirseq = nextseq (d,c)
        dirseq' = asum $ map (\d' -> lookup' d' edges) dirseq
    --liftIO $ print edges
    case dirseq' of
        -- dit zou als het goed is niet moeten kunnen met hoe de graaf in Codels wordt gemaakt
        Nothing -> error "doeStap: arcs heeft geen arc met huidige (dp,cc) of roterende dingen (i.e. geen uitgaande arcs)"
        Just ((d',c'),v') -> pure ((), v', c', d', s)


runProgram :: G.Vertex -> G.Graph [PInstruction] (DP,CC) -> IO ()
runProgram startnode graph = (\(a,v',c',d',s') -> ()) <$> runPiet runProgramLoop graph startnode CCLeft E []
