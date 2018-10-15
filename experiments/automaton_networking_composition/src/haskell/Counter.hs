{-
 -
 -}

module Counter (
  pop, push,
  count
) where

import Control.Monad.State
--import DataDefinitions (Stack, CounterState (CState) )
import DataDefinitions

pop :: State (Stack c) (Maybe c)
pop = state $ \a -> case a of []     -> (Nothing, [])
                              (x:xs) -> (Just x, xs)

push :: c -> State (Stack c) ()
push x = state $ \xs -> ((), x:xs)

{-
 - Function to increment a counter and its relevant state based depending on the
 - output of a function.
 -}
count :: Enum a => (a -> Bool) -> State (CounterState a) (Maybe a)
count f = state $ \(CState c cs) ->
                      case runState pop cs of
                        (Just c', cs') -> ((Just c'), (CState c cs'))
                        (Nothing, _)   ->
                            case f c of
                                True  -> let c' = succ c
                                         in ((Just c'), (CState c' cs))
                                False -> (Nothing, (CState c cs))


{- Some sample stateful operations
countTwice :: (FlowID -> Bool) -> (FlowID, FlowID)
countTwice f = let (Just a, s) = runState (count f) (CState minFlowID [])
                   (Just b, s') = runState (count f) (s)
               in (a, b)

pushTwice :: Stack Int -> ((Maybe Int, Maybe Int), Stack Int)
pushTwice = runState $
              push 3 >>
              pop >>= \x ->
                push 4 >>
                pop >>= \y ->
                  return (x, y)
-}
