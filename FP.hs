module FP (triple, collatz, halve, isLeapYear, find, isPangram, merge, encrypt, decrypt, increaseStock, decreaseStock, Outcome(..)) where

triple :: Num a => a -> a
triple x = x * 3

collatzHelper 1 count = count -- base case
-- divide n by 2 if even
-- 3n+1 if odd
-- add +1 to count each time then call collatzHelper with new args
collatzHelper n count = if even n then collatzHelper (n `div` 2) (count + 1) else collatzHelper (3 * n + 1) (count + 1)

collatz :: Integral a => a -> a
collatz x = collatzHelper x 0 -- count starts with 0

halve :: [a] -> ([a], [a])
-- take/drop y get the first/last y elements of xs in a tuple
halve xs = (take y xs, drop y xs) where y = (length xs) `div` 2

isLeapYear :: Integral a => a -> Bool
isLeapYear year = ((year `mod` 4) == 0) && (((year `mod` 100) /= 0) || ((year `mod` 400) == 0))

findHelper x xss index
    | index == (length xss) = -1
    | (xss !! index == x) = index
    | otherwise = findHelper x xss (index + 1)

-- find :: a -> [a] -> b
find x xss = findHelper x xss 0

isPangram :: [Char] -> Bool
-- create list of bools to check whether all of alphabet is in sentence, then check if that list of bools contains all True values
isPangram sentence = all (==True) [elem x sentence | x <- ['a'..'z']]

findHelper x xss index
    | index == (length xss) = -1
    | (xss !! index == x) = index
    | otherwise = findHelper x xss (index + 1)

-- find :: a -> [a] -> b
find x xss = findHelper x xss 0

merge :: Ord a => [a] -> [a] -> [a]
-- return current list if second list is empty
merge xs [] = xs
-- return current list if first list is empty
merge [] ys = ys
-- recusive calls to merge each sorted subsection of the lists
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

-- index 0 is 'a'
-- index 62 is '0'
chars = "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

-- recursive calls to shift chars by one until counter is 0
getNewValue character counter polarity
    | counter == 0 = character
    -- solve overlapping offsets (e.g 63, 128 etc are still the same character regardless of polarity)
    | otherwise = getNewValue (chars !! (if charIndex < 0 then charIndex + 63 else (if charIndex > 62 then charIndex - 63 else charIndex))) (counter - 1) polarity
    where
        -- shift current char index by 1
        charIndex = (polarity (find character chars) 1)

encrypt :: [Char] -> Int -> [Char]
encrypt xs offset = [getNewValue x offset (+) | x <- xs]

decrypt :: [Char] -> Int -> [Char]
decrypt xs offset = [getNewValue x offset (-) | x <- xs]


-- create datatype
data Outcome a b = Error a | Completion b deriving (Show)

-- create instances

instance Functor (Outcome a) where
  fmap _ (Error a) = Error a
  fmap f (Completion b) = Completion (f b)

instance Applicative (Outcome a) where
  pure = Completion
  (Error a) <*> _ = Error a
  (Completion f) <*> outcome = fmap f outcome

instance Monad (Outcome a) where
  return = Completion
  (Error a) >>= _ = Error a
  (Completion b) >>= f = f b

increaseStock :: Int -> Int -> Outcome String Int
increaseStock amount currentStock
    -- return Error if stock < 0
    | resultStock < 0 = Error "Negative stock level"
    | otherwise = Completion resultStock
    where
        resultStock = currentStock + amount

decreaseStock :: Int -> Int -> Outcome String Int
decreaseStock amount currentStock
    -- return Error if stock < 0
    | resultStock < 0 = Error "Negative stock level"
    | otherwise = Completion resultStock
    where
        resultStock = currentStock - amount
