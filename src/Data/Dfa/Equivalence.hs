{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Data.Dfa.Equivalence
Description : Various equivalence tests for Dfas.

The algorithms here are derived from various algorithms presented by
Hopcroft. Outside of his textbook, a few were taken from this paper:
https://arxiv.org/pdf/0907.5058.pdf
-}
module Data.Dfa.Equivalence
    ( checkDfa
    , equivalent
    , equivalentText
    , isomorphic
    , isomorphicText
    ) where

import           Data.Dfa
import           Data.Foldable              (forM_)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as S
import qualified Data.Text                  as T

import           Control.Applicative        ((<$>))
import           Control.Monad.Error.Class
import           Control.Monad.Identity     hiding (forM_)
import           Control.Monad.State.Strict hiding (forM_)

import           Parser.Dfa                 (doParseDfa)

data TagState
    = DfaA Int
    | DfaB Int
    deriving (Eq, Ord, Show)

isDfaA :: TagState -> Bool
isDfaA (DfaA _)
  = True
isDfaA _
  = False

isDfaB :: TagState -> Bool
isDfaB = not . isDfaA

untagState :: TagState -> Int
untagState (DfaA x)
  = x
untagState (DfaB x)
  = x

type SetOfSets a = S.Set (S.Set a)


isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

checkDfa :: T.Text -> Bool
checkDfa dfaText
  = isRight $ doParseDfa dfaText

compareText :: MonadError IOError m => (Dfa -> Dfa -> m Bool)
            -> T.Text -> T.Text -> m Bool
compareText f dfaT1 dfaT2
  = let dfaE1 = doParseDfa dfaT1
        dfaE2 = doParseDfa dfaT2
    in
      case doParseDfa dfaT1 of
        Left e -> throwError . userError . show $ e
        Right dfa1 ->
            case doParseDfa dfaT2 of
              Left e -> throwError . userError . show $ e
              Right dfa2 ->
                  f dfa1 dfa2

equivalentText :: MonadError IOError m => T.Text -> T.Text -> m Bool
equivalentText
  = compareText equivalent

isomorphicText :: MonadError IOError m => T.Text -> T.Text -> m Bool
isomorphicText
  = compareText isomorphic

equivalent :: MonadError IOError m => Dfa -> Dfa -> m Bool
equivalent
  = hopcroftKarp

isomorphic :: MonadError IOError m => Dfa -> Dfa -> m Bool
isomorphic dfa1 dfa2
  = do
      equiv <- equivalent dfa1 dfa2
      return $ _Q dfa1 == _Q dfa2 && equiv

-- | Destructive union within a set of sets
-- TODO: StateStack is static, so this might be refactorable to RWS
union :: (Show a, Ord a, MonadState (StateStack, SetOfSets a) m)
      => S.Set a -> S.Set a -> m Bool
union set1 set2
  | set1 == set2
      = return False
  | otherwise
      = do
          (x, curSets) <- get
          let removed = S.filter (\x -> (x /= set1) && (x /= set2)) curSets
              newSets = S.insert (set1 `S.union` set2) removed
          if curSets == removed
          then return False
          else do
              put (x, newSets)
              return True

-- TODO: StateStack is static, so this might be refactorable to RWS
find :: (Show a, Ord a, MonadState (StateStack, SetOfSets a) m)
     => a -> m (StateStack, S.Set a)
find element
  = do
      (x, sets) <- get
      let containingSets = S.filter (S.member element) sets
      if S.size containingSets > 1
      then error "BUG: Error in find --- too many sets"
      else
          return $ (x, setHead containingSets)


hasLefts :: [Either a b] -> Bool
hasLefts = or . map isLeft

lefts :: [Either a b] -> [a]
lefts = map unsafeLeft . filter isLeft

unsafeLeft :: Either a b -> a
unsafeLeft (Left x) = x

type StateStack = [(TagState, TagState)]

hopcroftKarp :: MonadError IOError m => Dfa -> Dfa -> m Bool
hopcroftKarp dfaA dfaB
  = let σ =  _Σ dfaA
        statesA = map (S.singleton . DfaA) [0.._Q dfaA - 1]
        statesB = map (S.singleton . DfaB) [0.._Q dfaB - 1]
        states' = S.fromList statesA `S.union` S.fromList statesB
        starts  = [(DfaA 0, DfaB 0)]
        states  = execState
                    (S.singleton (DfaA 0) `union`
                     S.singleton (DfaB 0))
                    (starts, states')
        (error, (_, partition)) = runState (forStack σ) states
    in case error of
         Left e -> throwError . userError $ e
         _ -> return $ (_Σ dfaA == _Σ dfaB) && checkPartition partition
  where
        forStack :: MonadState (StateStack, SetOfSets TagState) m
                 => S.Set Char
                 -> m (Either String ())
        forStack σ
          = do
              (stack, tagged) <- get
              let preStack = stack
              if null stack then return (Right ()) else do
                ret <- forM (S.toList σ) forSymbol
                (postStack, postTagged) <- get
                if (preStack /= postStack) then forStack σ else
                  if hasLefts ret
                  then return $ Left $ foldr (++) "" (lefts ret)
                  else return $ Right ()

        forSymbol :: MonadState (StateStack, SetOfSets TagState) m
                  => Char
                  -> m (Either String ())
        forSymbol symb
          = do
              ((p, q):stack, tagged) <- get
              let pNum = untagState p
                  qNum = untagState q
                  err = ", " ++ show symb ++ ") doesn't exist.\n"
                  err1 = "δ(" ++ show pNum ++ err
                  err2 = "δ(" ++ show pNum ++ err
                  qOnSymbNum = M.lookup (qNum, symb) (_δ dfaB)
              case M.lookup (pNum, symb) (_δ dfaA) of
                Nothing -> return $ Left err1
                (Just pOnSymbNum) ->
                    case M.lookup (qNum, symb) (_δ dfaB) of
                      Nothing -> return $ Left err2
                      (Just qOnSymbNum) -> do
                          let pOnSymb = DfaA pOnSymbNum
                              qOnSymb = DfaB qOnSymbNum
                          p' <- snd <$> find pOnSymb
                          q' <- snd <$> find qOnSymb
                          when (p' /= q') $ do
                              _ <- union p' q'
                              (stack', set) <- get
                              put $ ((pOnSymb, qOnSymb):stack', set)
                          return (Right ())

        checkPartition :: SetOfSets TagState -> Bool
        checkPartition
          = S.foldr (&&) True . S.map sameFinality

        sameFinality :: S.Set TagState -> Bool
        sameFinality states
          = let aStates' = S.filter isDfaA states
                bStates' = S.filter isDfaB states
                unTag = S.map untagState
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
