-- Problem 18: Maximum path sum I
-- By starting at the top of the triangle below and moving to adjacent
-- numbers on the row below, the maximum total from top to bottom is 23.
--                                     3
--                                    7 4
--                                   2 4 6
--                                  8 5 9 3
-- That is, 3 + 7 + 4 + 9 = 23.
-- Find the maximum total from top to bottom of the triangle below:
--                                     75
--                                   95 64
--                                  17 47 82
--                                18 35 87 10
--                               20 04 82 47 65
--                             19 01 23 75 03 34
--                            88 02 77 73 07 63 67
--                          99 65 04 28 06 16 70 92
--                         41 41 26 56 83 40 80 70 33
--                       41 48 72 33 47 32 37 16 94 29
--                      53 71 44 65 25 43 91 52 97 51 14
--                    70 11 33 28 77 73 17 78 39 68 17 57
--                   91 71 52 38 17 14 91 43 58 50 27 29 48
--                 63 66 04 68 89 53 67 30 73 16 69 87 40 31
--                04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
-- NOTE: As there are only 16384 routes, it is possible to solve this problem
--    by trying every route. However, Problem 67, is the same challenge with
-- a triangle containing one-hundred rows; it cannot be solved by brute
-- force, and requires a clever method! ;o)
type Triangle = [[Int]]

triangle :: Triangle
triangle =
  map ((map read) . words)
      ["75"
      ,"95 64"
      ,"17 47 82"
      ,"18 35 87 10"
      ,"20 04 82 47 65"
      ,"19 01 23 75 03 34"
      ,"88 02 77 73 07 63 67"
      ,"99 65 04 28 06 16 70 92"
      ,"41 41 26 56 83 40 80 70 33"
      ,"41 48 72 33 47 32 37 16 94 29"
      ,"53 71 44 65 25 43 91 52 97 51 14"
      ,"70 11 33 28 77 73 17 78 39 68 17 57"
      ,"91 71 52 38 17 14 91 43 58 50 27 29 48"
      ,"63 66 04 68 89 53 67 30 73 16 69 87 40 31"
      ,"04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"]

testTri :: Triangle
testTri =
  map ((map read) . words)
      ["3","7 4","2 4 6","8 5 9 3"]

data Move
  = L
  | R
  deriving ((((((((((((((((((Show))))))))))))))))))

type Path = [Move]

type Pos = (Int,Int)

-- get value at a given position (col,row)
getPos :: Triangle -> Pos -> Int
getPos t (x,y) = (t !! y) !! x

-- "previous positions", positions from the prior row that can reach this
-- position
prevPosns :: Pos -> [(Move,Pos)]
prevPosns (0,y) = [(L,(0,y - 1))]
prevPosns (x,y) =
  [(R,(x - 1,y - 1))] ++
  if x /= y
     then [(L,(x,y - 1))]
     else []

-- selects the best of paths from previous positions
bestOfPrev :: Triangle -> Pos -> (Path,Int)
bestOfPrev t p =
  case paths of
    [x] -> x
    [(p1,d1),(p2,d2)] ->
      if (d1 > d2)
         then (p1,d1)
         else (p2,d2)
  where paths =
          map (possMove t)
              (prevPosns p)

possMove :: Triangle -> (Move,Pos) -> (Path,Int)
possMove t (m,pos) = (m : p,d)
  where (p,d) = maxPath t pos

maxPath :: Triangle -> Pos -> (Path,Int)
maxPath t p =
  case p of
    (0,0) -> ([],curval)       -- first node
    (x,_) -> (pprev,d + curval)
  where (pprev,d) = bestOfPrev t p
        curval = getPos t p

-- bestPath triangle
bestPath :: Triangle -> (Maybe Path,Int)
bestPath t = (lookup max (zip ds ps),max)
  where l = length t
        (ps,ds) =
          unzip (map (maxPath t)
                     (zip [0 .. l - 1]
                          (replicate l (l - 1))))
        max = maximum ds