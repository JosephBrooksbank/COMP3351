module A4 where
-- assuming this is necessary, it wasn't in the instructions 
import Data.List
import Data.Char


-- |onlyLowercase: returns a list of items which begin with a lowercase
-- @list: list of strings to parse
-- @return: items from list that began lowercase
onlyLowercase list  = filter (\x -> isLower (head x)) list


-- |longerWord: Helper function for longestString. Returns the longer of two words
-- @return: the longer string between @wordOne and @wordTwo
longerWord wordOne wordTwo = if (length wordOne > length wordTwo)
  then wordOne else wordTwo


-- |longestString: finds longest string in list
-- @list: the list to parse
-- @return: the longest string found in the list
longestString list = foldl longerWord "" list

-- | multpair: helper function for multipling tuples
multpair (a,b) = a*b
-- | multpairs: multiplies tuples together, returns list of product
-- @list: the list of tuples to parse
-- @return: a new list, containing the products of the tuples
multpairs list = map multpair list

-- |sqsum: squares each item in @list, then adds the squares together
-- @return: the sum of squares of all items found in list
sqsum list = foldl (+) 0 (map (\x -> x*x) list)

-- |duplist: takes a list and duplicates every item in it
-- @list: the list to make duplicates of
-- @return: a new list containing two of every item in original list.
duplist list = foldl (++) [] (map (\x -> [x,x]) list)
