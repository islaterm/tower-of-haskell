-- "One way or another" (c) by Ignacio Slater M.

-- "One way or another" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.
{-# LANGUAGE FlexibleInstances #-}

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

cnf :: Conf
cnf L = [1 .. 5]
cnf C = []
cnf R = []
