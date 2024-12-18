import Control.Applicative (Applicative (liftA2), liftA3)
import Control.Monad (ap, join)
import Data.Char (isNumber, toLower, toUpper)
import Data.Function (on)
import Data.List (groupBy, inits, isPrefixOf, isSuffixOf, sort, tails, transpose)
import Data.Maybe (mapMaybe)
import Data.Monoid (All (All, getAll))
import GHC.IO (unsafePerformIO)
import Text.Read (readMaybe)
import Text.Regex.TDFA

{-# NOINLINE input #-}
input = unsafePerformIO . readFile . ("./inputs/" ++) . (++ ".txt")

readInts :: String -> [Int]
readInts = mapMaybe readMaybe . groupBy ((==) `on` isNumber)

rows = transpose . map readInts . lines

(.:) = (.) . (.)

difference = abs .: (-)

solve1a =
  sum
    . diff
    . map sort
    . rows
 where
  diff = zipWith difference <$> head <*> last

-- >>> solve1a $ input "1"
-- 2367773

solve1b =
  sum
    . map sum
    . occurences
    . rows
 where
  occurences = ap (map . (. (==)) . flip filter . last) head

-- >>> solve1b $ input "1"
-- 21271939

verify = compare <*> tail
 where
  sorted = ((||) <$> and <*> all not) .: zipWith (>)
  compare = getAll .: (All .: sorted <> (All . and) .: zipWith inRange)
  inRange = flip elem [1 .. 3] .: difference

countTrue = length . filter id

solve2a = countTrue . map (verify . readInts) . lines

-- >>> solve2a $ input "2"
-- 269

solve2b = countTrue . map (any verify . (<$> removes) . flip id . readInts) . lines
 where
  removes = map (take <> drop . (+ 1)) [0 .. 10]

-- >>> solve2b $ input "2"
-- 337

solve3a = sum . map (product . readInts) . occurences
 where
  occurences = getAllTextMatches . (=~ pattern) :: String -> [String]
  pattern = "mul\\([0-9]+,[0-9]+\\)"

dropEnd = (. (reverse . inits)) . flip (!!)

-- >>> snagg "a"
-- ["A","a"]

solve4a = sum . map count . sequence transformations . lines
 where
  diagonal = transpose . zipWith drop [0 ..]
  diag = transpose . reverse . zipWith (++) pyramid . reverse
  pyramid = map (`replicate` '.') [0 ..]
  count = sum . map (length . filter ("XMAS" `isPrefixOf`) . tails)

  transformations =
    liftA2
      (.)
      [id, map reverse]
      [id, transpose, diag, reverse . diag . reverse]

-- >>> solve4a $ input "4"
-- 2633
