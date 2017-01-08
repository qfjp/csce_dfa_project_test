{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Data.Dfa.Equivalence
Description : Various equivalence tests for Dfas.

The algorithms here are derived from various algorithms presented by
Hopcroft. Outside of his textbook, a few were taken from this paper:
https://arxiv.org/pdf/0907.5058.pdf
-}
module Data.Dfa.Equivalence (hopcroftKarp) where

import           Data.Dfa
import           Data.Foldable          (forM_)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust, fromMaybe)
import qualified Data.Set               as S

import           Control.Monad          (unless, when)
import           Control.Monad.Identity hiding (forM_)
import           Control.Monad.State    hiding (forM_)

data TagState
    = TagState Char Int
    deriving (Eq, Ord, Show)

type SetOfSets a = S.Set (S.Set a)

-- | Destructive union within a set of sets
union :: (Show a, Ord a) => S.Set a -> S.Set a -> State (SetOfSets a) Bool
union set1 set2
  | set1 == set2
      = return False
  | otherwise
      = do
          curSets <- get
          let removed = S.filter (\x -> (x /= set1) && (x /= set2)) curSets
              newSets = S.insert (set1 `S.union` set2) removed
          if curSets == removed
          then return False
          else do
              put newSets
              return True

find :: (Show a, Ord a) => a -> State (SetOfSets a) (S.Set a)
find element
  = do
      sets <- get
      let containingSets = S.filter (S.member element) sets
      if S.size containingSets > 1
      then error "BUG: Error in find --- too many sets"
      else
          return $ setHead containingSets


hopcroftKarp :: Dfa -> Dfa -> Bool
hopcroftKarp dfaA dfaB
  = let σ =  _Σ dfaA
        statesA = map (S.singleton . TagState 'A') [0.._Q dfaA - 1]
        statesB = map (S.singleton . TagState 'B') [0.._Q dfaB - 1]
        states' = S.fromList statesA `S.union` S.fromList statesB
        starts  = (TagState 'A' 0, TagState 'B' 0)
        states  = execState
                    (union (S.singleton (TagState 'A' 0))
                           (S.singleton (TagState 'B' 0)))
                    states'
        partition = execState (evalStateT (forStack σ) [starts]) states
    in (_Σ dfaA == _Σ dfaB) && checkPartition partition
  where
        forStack :: S.Set Char
                 -> StateT [(TagState, TagState)] (State (SetOfSets TagState)) ()
        forStack σ
          = do
              stack <- get
              let preStack = stack
              unless (null stack) $ do
                  forM_ σ forSymbol
                  partition <- lift get
                  postStack <- get
                  unless (preStack == postStack) $ forStack σ

        forSymbol :: Char
                  -> StateT [(TagState, TagState)] (State (SetOfSets TagState)) ()
        forSymbol symb
          = do
              ((p@(TagState _ pNum), q@(TagState _ qNum)):stack) <- get
              let pOnSymbNum = fromJust $ M.lookup (pNum, symb) (_δ dfaA)
                  qOnSymbNum = fromJust $ M.lookup (qNum, symb) (_δ dfaB)
                  pOnSymb = TagState 'A' pOnSymbNum
                  qOnSymb = TagState 'B' qOnSymbNum
              p' <- lift $ find pOnSymb
              q' <- lift $ find qOnSymb
              when (p' /= q') $ do
                  lift $ union p' q'
                  put $ (pOnSymb, qOnSymb):stack

        checkPartition :: SetOfSets TagState -> Bool
        checkPartition
          = S.foldr (&&) True . S.map sameFinality

        sameFinality :: S.Set TagState -> Bool
        sameFinality states
          = let aStates' = S.filter (\(TagState chr num) -> chr == 'A') states
                bStates' = S.filter (\(TagState chr num) -> chr == 'B') states
                unTag = S.map (\(TagState chr num) -> num)
                aStates = unTag aStates'
                bStates = unTag bStates'
                aFinal = allFinal dfaA aStates
                aNonFinal = allNonFinal dfaA aStates
                bFinal = allFinal dfaB bStates
                bNonFinal = allNonFinal dfaB bStates
            in (aFinal && bFinal) || (aNonFinal && bNonFinal)

allFinal :: Dfa -> S.Set Int -> Bool
allFinal dfa states
  = states `S.isSubsetOf` final
  where final = _F dfa

allNonFinal :: Dfa -> S.Set Int -> Bool
allNonFinal dfa states
  = S.null $ S.intersection final states
  where final = _F dfa

setHead :: S.Set a -> a
setHead
  = head . S.toAscList
