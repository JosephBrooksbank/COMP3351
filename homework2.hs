module A2 where


-- | findMin:: Finds the minimum value in the given list
-- @theList --  a list to find the minimum of
-- @return an int which is the minimum value found
findMin theList = if theList == []
                  then error "empty list"
                  else if length theList == 1
                    then head theList
                  else if (head theList < findMin (tail theList))
                    then head theList
                    else findMin (tail theList)


-- | tupleDotProduct:: Computes the dot product for the two given lists
-- @firstList -- the first list to multiply
-- @secondList -- the second list to multiply with the first
-- @return an int which is the value of the dot product of the two lists
tupleDotProduct firstList secondList = if firstList == [] || secondList == []
                                        then 0
                                        else
                                          (head firstList) * (head secondList)
                                          + tupleDotProduct (tail firstList) (tail secondList)

-- | revZip2Lists:: reverse zips two lists
-- @firstList -- the first list to be zipped
-- @secondList -- the second list to be zipped
-- @return -- a list of tuples(pairs), each containing one value from the above two list
-- and in backwards order
revZip2Lists  firstList secondList = if firstList == [] || secondList == []
                                    then error "empty list"
                                    else if length firstList /= length secondList
                                      then error "nonmatched sizes"
                                      else if length firstList == 1
                                        then [(head secondList, head firstList)]
                                      else revZip2Lists (tail firstList) (tail secondList) ++ [(head secondList, head firstList)]


-- | everyThird:: Takes every third element of the list
-- @theList -- the input list
-- @return a list containing every third value from @theList
everyThird theList=
                    --  if length theList == 3 then [head theList]
                      if length theList < 3
                        then []
                      else [head (tail (tail theList))] ++ everyThird (tail (tail (tail theList)))
