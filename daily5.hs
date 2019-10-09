module A5 where



-- | comparison function, maintains first found of length if multiple in list
longerWord' wordOne wordTwo = if (length wordTwo < length wordOne)
    then wordOne else wordTwo

-- | comparison, maintains last in list if multiple of same length found 
longerWord wordOne wordTwo = if (length wordTwo > length wordOne)
    then wordTwo else wordOne



    -- |longestString: finds longest string in list, returning first if multiple have same length
    -- @list: the list to parse
    -- @return: the longest string found in the list
longestString list = foldl longerWord "" list

    -- |longestString': finds longest string in list, returning last if multiple have same length
    -- @list: the list to parse
    -- @return: the longest string found in the list
longestString' list = foldl longerWord' "" list


    -- |longestStringHelper: finds longest string in list, returns {first|last} if multiple, depending on which function is passed for function
longestStringHelper function list = foldl function "" list

    -- |longestString3: Same functionality as longerString
longestString3 list = longestStringHelper longerWord list

    -- |longestString4: Same functionality as longerString'
longestString4 list = longestStringHelper longerWord' list
