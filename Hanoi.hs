-- "One way or another" (c) by Ignacio Slater M.

-- "One way or another" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.Core.QuickCheck     ( modifyMaxSize )

data Peg = L | C | R deriving (Eq, Show)
type Disk = Int
type Conf = Peg -> [Disk]
type Move = (Peg, Peg)

instance Show Conf where
  show c = show (c L, c C, c R)

{-|
Pushes a disk into a peg.
-}
push :: Disk -> Peg -> Conf -> Conf
push disk peg conf | null (conf peg) = newConf
                   | disk < head (conf peg) = newConf
                   | otherwise = error "The pushed disk needs to be smaller"
 where
  newConf L = if peg == L then disk : conf L else conf L
  newConf C = if peg == C then disk : conf C else conf C
  newConf R = if peg == R then disk : conf R else conf R

{-|
Pops a disk from a peg
-}
pop :: Peg -> Conf -> (Disk, Conf)
pop peg conf
  | not (null (conf peg)) = (head (conf peg), newConf)
  | otherwise = error ("Can't pop a disk from empty peg " ++ show peg)
 where
  newConf L = if peg == L then tail (conf L) else conf L
  newConf C = if peg == C then tail (conf C) else conf C
  newConf R = if peg == R then tail (conf R) else conf R

{-|
Moves a disk to a new peg.
This function throws an error if an illegal movement is given.
-}
step :: Move -> Conf -> Conf
step (origin, destination) conf = push popDisk destination popConf
 where
  popResult = pop origin conf
  popDisk   = fst popResult
  popConf   = snd popResult

{-|
Generates the optimal sequence of movements to complete the game.
-}
optStrategy :: Int -> Move -> Conf -> [(Move, Conf)]
optStrategy disks (source, dest) conf = if disks == 1
  then [(move, step move conf)]
  else firstMove ++ secondMove ++ lastMove
 where
  move  = (source, dest)
  spare = if source == L || dest == L
    then if source == C || dest == C then R else C
    else L
  firstMove        = optStrategy (disks - 1) (source, spare) conf
  confAfterFstMove = snd (last firstMove)
  secondMove       = [(move, step move confAfterFstMove)]
  confAfterSndMove = snd (last secondMove)
  lastMove         = optStrategy (disks - 1) (spare, dest) confAfterSndMove

makeInit :: Int -> Peg -> Conf
makeInit n p p' | p' == p   = [1..n]
                | otherwise = []

play :: Int -> Peg -> Peg -> IO()
play n s t = putStr $ show initConf ++ foldr f v (optStrategy n (s,t) initConf) where
  initConf  = makeInit n s
  v         = []
  f (m,c) r = "\n -> " ++ show m ++ " -> " ++ show c ++ r
  
others :: Peg -> (Peg, Peg)
others L = (R, C)
others C = (L, R)
others R = (L, C)

instance {-# OVERLAPPING #-} Arbitrary Move where
  arbitrary = do
    s <- frequency [(1, return L), (1, return C), (1, return L)]
    t <- let (x1, x2) = others s in frequency [(1, return x1), (1, return x2)]
    return (s, t)

isSorted :: [Disk] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = x < head xs && isSorted xs

isValid :: [(Move, Conf)] -> Bool
isValid [] = True
isValid (x:xs) = isSorted (conf L) && isSorted (conf C) && isSorted (conf R) && isValid xs
  where conf = snd x

testoptStrategy :: Spec
testoptStrategy =
  describe "Optimal strategy for Hanoi Tower:" $ modifyMaxSize (const 10) $ do
    it "Configuraciones generadas son validas"
      $
        property
      $ \n (s, t) -> 1 <= n ==> isValid (optStrategy n (s, t) (makeInit n s))
    it "Tamaño de la estrategia optima"
      $
        property
      $ \n (s, t) -> 1 <= n ==> length (optStrategy n (s, t) (makeInit n s)) == 2^n - 1