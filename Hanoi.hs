-- "One way or another" (c) by Ignacio Slater M.

-- "One way or another" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.
{-# LANGUAGE FlexibleInstances #-}

data Peg = L | C | R deriving (Eq, Show)
type Disk = Int
type Conf = Peg -> [Disk]
type Move = (Peg,Peg)

instance Show Conf where
  show c = show (c L, c C, c R)

{-|

-}
push :: Disk -> Peg -> Conf -> Conf
push _ _ _ = error "Not implemented"
-- step :: Move -> Conf -> Conf
-- step (origin)