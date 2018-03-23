-- Problem 19: Counting Sundays
-- You are given the following information, but you may prefer to do some
-- research for yourself.
--   * 1 Jan 1900 was a Monday.
--   * Thirty days has September,
--     April, June and November.
--     All the rest have thirty-one,
--     Saving February alone,
--     Which has twenty-eight, rain or shine.
--     And on leap years, twenty-nine.
--   * A leap year occurs on any year evenly divisible by 4, but not on a
--     century unless it is divisible by 400.
-- How many Sundays fell on the first of the month during the twentieth
-- century (1 Jan 1901 to 31 Dec 2000)?
isLeapYear :: Int -> Bool
isLeapYear n = ed 4 && ((not . ed) 100 || ed 400)
  where ed = \d -> mod n d == 0

data Month
  = Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving (Show,Eq,Ord)

data WDay
  = Sun
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  deriving (Show,Eq)

type Date = Int

type Year = Int

data Day =
  Day WDay
      Date
      Month
      Year
  deriving ((((Show))))

numOfDays :: Month -> Year -> Int
numOfDays m y
  | elem m [Sep,Apr,Jun,Nov] = 30
  | m == Feb =
    if isLeapYear y
       then 29
       else 28
  | otherwise = 31

daysYM :: Year -> Month -> [(Date,Year,Month)]
daysYM y m = (,,) <$> [1 .. numOfDays m y] <*> [y] <*> [m]

firstDay :: Day
firstDay = Day Mon 1 Jan 1900

years :: [Year]
years = [1900 ..]

months :: [Month]
months = [Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec]

wdays :: [WDay]
wdays = [Mon,Tue,Wed,Thu,Fri,Sat,Sun]

days :: [(Date,Year,Month)]
days = concat (daysYM <$> years <*> months)

-- length (filter (\(Day wd d m y) -> wd == Sun && d == 1 && y >= 1901 && y <= 2000) (takeWhile (\(Day _ _ _ y) -> y <= 2018) fullDays))
fullDays :: [Day]
fullDays =
  map (\(wd,(d,y,m)) -> Day wd d m y)
      (zip (cycle wdays) days)