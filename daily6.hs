module A6 where


-- | firstAnswer: Returns first non Nothing answer in list when operated on by funcion
-- @function: The function to apply to the elements of the list
-- @list: the list to apply the function to
-- @return: The first non nothing answer
firstAnswer :: (a -> Maybe b) -> [a] -> Maybe b
firstAnswer function list = case (function (head list)) of
          Nothing ->  (firstAnswer function (tail list))
          Just b -> Just b


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
