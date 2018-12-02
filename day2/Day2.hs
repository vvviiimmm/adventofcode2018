import Control.Monad as M
import Data.List as List
import Data.Map.Strict as Map

{-
--- Day 2: Inventory Management System ---
You stop falling through time, catch your breath, and check the screen on the device. "Destination reached. Current Year: 1518. Current Location: North Pole Utility Closet 83N10." You made it! Now, to find those anomalies.

Outside the utility closet, you hear footsteps and a voice. "...I'm not sure either. But now that so many people have chimneys, maybe he could sneak in that way?" Another voice responds, "Actually, we've been working on a new kind of suit that would let him fit through tight spaces like that. But, I heard that a few days ago, they lost the prototype fabric, the design plans, everything! Nobody on the team can even seem to remember important details of the project!"

"Wouldn't they have had enough fabric to fill several boxes in the warehouse? They'd be stored together, so the box IDs should be similar. Too bad it would take forever to search the warehouse for two similar box IDs..." They walk too far away to hear any more.

Late at night, you sneak to the warehouse - who knows what kinds of paradoxes you could cause if you were discovered - and use your fancy wrist device to quickly scan every box and produce a list of the likely candidates (your puzzle input).

To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number that have an ID containing exactly two of any letter and then separately counting those with exactly three of any letter. You can multiply those two counts together to get a rudimentary checksum and compare it to what your device predicts.

For example, if you see the following box IDs:

abcdef contains no letters that appear exactly two or three times.
bababc contains two a and three b, so it counts for both.
abbcde contains two b, but no letter appears exactly three times.
abcccd contains three c, but no letter appears exactly two times.
aabcdd contains two a and two d, but it only counts once.
abcdee contains two e.
ababab contains three a and three b, but it only counts once.
Of these box IDs, four of them contain a letter which appears exactly twice, and three of them contain a letter which appears exactly three times. Multiplying these together produces a checksum of 4 * 3 = 12.

What is the checksum for your list of box IDs?
-}

readLines :: FilePath -> IO [String]
readLines f = fmap lines (readFile f)

-- Part 1

insertOrUpdate :: (Ord k, Num v) => k -> Map k v -> Map k v
insertOrUpdate key m = Map.alter fn key m
  where
    fn maybeValue =
      case maybeValue of
        Just val -> Just (val + 1)
        Nothing -> Just 1

-- Returns the number of exactly 2 and 3 occurences of any character, e.g (1, 1) means there
-- was 1 character with 2 occurences and 1 character with 3 occurences 
counts :: (Ord a) => [a] -> (Int, Int)
counts xs =
  ( (checksumNumber $ List.elem 2 occList)
  , (checksumNumber $ List.elem 3 occList))
  where
    occurencesMap = List.foldr insertOrUpdate Map.empty xs
    occList = Map.elems occurencesMap
    checksumNumber pred =
      if (pred)
        then 1
        else 0


-- Sums up the number of 2and3 occurences and multiplies them to get the result
checksum :: (Ord a) => [[a]] -> Int
checksum xs =
  case (List.foldr (\(l, r) (al, ar) -> (al + l, ar + r)) (0, 0) $
        fmap counts xs) of
    (0, 0) -> 0
    (l, 0) -> l
    (0, r) -> r
    (l, r) -> l * r

part1 :: IO ()
part1 = readLines "input.txt" >>= \lns -> print $ checksum lns

{-
Part 2
Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.

The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:

abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.

What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)
-}

-- Two words are similar if they differ in a single character, all other characters
-- are the same and on the same positions.
-- "abcd" and "atcd" are similar
-- "abcd" and "abty" are not
similar :: (Eq a) => [a] -> [a] -> Bool
similar left right = similar' 0 left right
  where
    similar' _ [] [] = True
    similar' 2 _ _ = False
    similar' n (x:xs) (y:ys) =
      if (x == y)
        then similar' n xs ys
        else similar' (n + 1) xs ys

-- Difference in two similar words, skips the characters that are different
-- diff "abcd" "apcd" == "acd"
diff :: (Eq a) => [a] -> [a] -> [a]
diff (x:xs) (y:ys) =
  if (x == y)
    then x : (diff xs ys)
    else diff xs ys
diff [] ys = ys
diff xs [] = xs

commonBoxLetters (x:xs) =
  case M.msum
         (fmap
            (\a ->
               if (similar x a)
                 then Just (diff x a)
                 else Nothing)
            xs) of
    Just a -> a
    Nothing -> commonBoxLetters xs
commonBoxLetters [] = ""

part2 :: IO ()
part2 = readLines "input.txt" >>= \lns -> print $ commonBoxLetters lns