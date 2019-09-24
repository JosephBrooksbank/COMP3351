module A3 where


-- | removeAllExcept -- Removes all items from list except specified item
-- @item : the item to leave in the list
-- @theList : the list to modify
-- @return a list containing only the items of value @item from @theList
  removeAllExcept item theList =
      if theList == []
        then []
      else
        if (head theList)  == item
          then
            [head theList] ++ removeAllExcept item (tail theList)
          else
            removeAllExcept item (tail theList)

-- | removeAll -- Removes all instances of @item from @theList
-- @item -- the item to remove from the list
-- @theList -- the list to modify
-- @return a list containing the items found in @theList, without instances
-- of @item
  removeAll item theList =
        if theList == []
          then []
        else
          if (head theList)  == item
            then
              removeAll item (tail theList)
            else
              [head theList] ++ removeAll item (tail theList)


-- | substitute -- substitutes @second for @first in @theList
-- @first -- the item to replace
-- @second -- the item to replace with
-- @theList -- the list of objects which may be modified
-- @return the list found in @theList but with every instance of @first
-- replaced with @second
  substitute first second theList =
        if theList == []
          then []
        else
          if (head theList) == first
            then
              [second] ++ substitute first second (tail theList)
          else
            [head theList] ++ substitute first second (tail theList)


-- | mergeSorted3 -- Combines 3 previously sorted lists
-- @firstList the first list
-- @secondList the second list
-- @thirdList the third list
-- @note the instructions say to describe all function parameters, but is this
-- REALLY necessary?
-- @return a single list containing the other three list values, sorted in one list
  mergeSorted3 firstList secondList thirdList =
    -- merging two first, then that list with the last
    mergeSorted2 (mergeSorted2 firstList secondList) thirdList


-- | mergeSorted2 -- Combines two previously sorted lists
-- @firstList -- the first list to combine
-- @secondList -- the second list to combine
-- @return a single sorted list composed of the values from the other two 
  mergeSorted2 firstList secondList =
    if firstList == [] && secondList == []
      then []
    else if firstList == []
      then secondList
    else if secondList == []
      then firstList
    else if (head firstList) <= (head secondList)
      then [head firstList] ++ mergeSorted2 (tail firstList) secondList
    else
      [head secondList] ++ mergeSorted2 firstList (tail secondList)
