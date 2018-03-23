import Data.List
import Data.Maybe

-- Problem 15: Lattice paths
-- Starting in the top left corner of a 2 * 2 grid, there are 6 routes
-- (without backtracking) to the bottom right corner.
-- How many routes are there through a 20 * 20 grid?
data Coords
  = C Int
      Int
  | End
  deriving ((((((((((((((((Show))))))))))))))))

instance Eq Coords where
  C x1 y1 == C x2 y2 = (x1,y1) == (x2,y2)
  End == _ = False
  _ == End = False

instance Ord Coords where
  compare (C x1 y1) (C x2 y2) =
    compare (x1,y1)
            (x2,y2)
  compare End _ = LT
  compare _ End = LT

type Path = (Coords,Int)

-- type NewPath = (Coords,Coords,Int)
data Dir
  = Rght
  | Down
  deriving (((((((((((((((((Eq)))))))))))))))))

dirs :: [Dir]
dirs = [Rght,Down]

move
  :: (Int,Int) -> Maybe Path -> Dir -> Maybe Path
move _ Nothing _ = Nothing
move _ (Just (End,n)) _ = Just (End,n)
move (dx,dy) (Just (C x y,n)) dir
  | dx == x && dy == y = Just (End,n) -- reached the end
  | dx == x && dir == Rght = Nothing  -- illegal move
  | dy == y && dir == Down = Nothing  -- illegal move
  | dir == Rght = Just (C (x + 1) y,n) -- right
  | dir == Down = Just (C x (y + 1),n) -- down

isEnd :: Maybe Path -> Bool
isEnd (Just (End,_)) = True
isEnd _ = False

cmpFst :: Eq a
       => Maybe (a,b) -> Maybe (a,b) -> Bool
cmpFst (Just (x,_)) (Just (y,_)) = x == y

-- filter invalid ("Nothing") paths, and
-- consolidate paths with same endpoint
consolidate :: [Maybe Path] -> [Maybe Path]
consolidate =
  (map addPaths) .
  (groupBy cmpFst) . sort . (filter (not . isNothing)) . (filter (not . isEnd))

addPaths :: [Maybe Path] -> Maybe Path
addPaths ps = foldl1 (\(Just (x,n1)) (Just (_,n2)) -> (Just (x,n1 + n2))) ps

moves :: (Int,Int) -> [[Maybe Path]]
moves ds =
  (iterate (moveF ds)
           [Just (C 0 0,1)])

moveF
  :: (Int,Int) -> [Maybe Path] -> [Maybe Path]
moveF ds p = consolidate (move ds <$> p <*> dirs)

getN :: Maybe Path -> Int
getN (Just (_,n)) = n
getN Nothing = 0

-- numPaths (20,20)
numPaths :: (Int,Int) -> Int
numPaths ds =
  (getN . head . last . (takeWhile (\x -> length x > 0))) (moves ds)