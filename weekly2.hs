module W2 where

-- | allAnswers': Helper function for allAnswers, acculumates values
allAnswers' function list accuList = case list of
        [] -> Just accuList
        list ->
          case (function (head list)) of
            Nothing -> Nothing
            Just b -> allAnswers' function (tail list) (accuList ++ b)


-- | allAnswers: combines all Just objects into a single list when returned by function
-- @function: The function to apply to the elements of the list
-- @list: the list to appl the function to
-- @return: A list of Just objects from the earlier lists
-- E.g. allAnswers (id) [Just [1,2], Just[3], Just [4,5]] -> Just [1,2,3,4,5]
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers function list = allAnswers' function list []


-- | firstAnswer: Returns first non Nothing answer in list when operated on by funcion
-- @function: The function to apply to the elements of the list
-- @list: the list to apply the function to
-- @return: The first non nothing answer
firstAnswer :: (a -> Maybe b) -> [a] -> Maybe b
firstAnswer function list = case (function (head list)) of
          Nothing ->  (firstAnswer function (tail list))
          Just b -> Just b



data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)


variablesToString patt = case patt of
                    VariablePat string -> [string]
                    ConstructorPat (string, patt) -> variablesToString patt
                    TuplePat (patt) -> foldl (\x y -> (variablesToString y) ++ x) [] patt
                    _ -> []


uniqueString list = if (elem (head list) (tail list)) then False
       else uniqueString (tail list)



checkPat patt = uniqueString (variablesToString patt)

-- //// NOTE: DISCUSSED PARTS OF MATCH with Ryan Dunagan

-- | match: matches the value given with the pattern given
-- @val: the value to pattern match
-- @patt: the pattern to pattern match against
-- @return: the valid patterns for each value supplied
match (val, patt) = case patt of
  -- Wildcard -> just [] as specified
  WildcardPat -> Just []
  -- VariablePat -> if variable, match to value
  VariablePat s ->  Just [(s, val)]
  -- UnitPat -> if Value is unit, then just []
  UnitPat -> if (val == Unit) then Just [] else Nothing
  -- ConstantPat -> if value is a constant of same value, then just []
  ConstantPat int1 -> case val of
      Constant int2 -> if (int1 == int2) then Just [] else Nothing
      _ -> Nothing
  -- ConstructorPat -> if value is a constructor with same Pattern type, match recursively with those patterns
  ConstructorPat (s1, patt2) -> case val of
        Constructor (s2,val2) -> if (s1 == s2 && (match (val2,patt2)/= Nothing))
          then (match (val2, patt2)) else Nothing
        _ -> Nothing

  -- TuplePat -> If value is a Tuple of equal length, match each tuple item with tuplepat item
  TuplePat ps -> case val of
    Tuple vs -> if ((length ps) == (length vs) && allAnswers match (zip vs ps) /= Nothing)
                    then allAnswers match (zip vs ps)
                    else Nothing

-- | firstMatch : Returns first valid pattern for the Value supplied
-- @val: The value to check against the patterns
-- @list: the list of patterns to check against
-- @return: the first valid pattern that the value matches 
firstMatch val list =  firstAnswer (\x -> match (val,x)) list
