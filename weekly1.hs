
data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a)
  deriving(Show)
instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False


-- | nodeValue: Returns the value of the given node
-- @aTree: The node to get the value of
-- @return: The value of the node
nodeValue aTree = case aTree of
                  EmptyNode -> error("Empty Node, no value to return")
                  TriNode a left middle right -> a

-- | (left|right|middle)Child: Returns the (left|right|middle) child node
-- @aTree: The parent node
-- @return: (left|right|middle) node
leftChild aTree = case aTree of
                  EmptyNode -> error("Empty Node")
                  TriNode a left middle right -> left

middleChild aTree = case aTree of
                  EmptyNode -> error("Empty Node")
                  TriNode a left middle right -> middle
rightChild aTree = case aTree of
                  EmptyNode -> error("Empty Node")
                  TriNode a left middle right -> right



-- | inTree: Searches tree for value given
-- @a: The value to search for
-- @aTree: The tree to search through
-- @return: True if the value was found
inTree aVal aTree = case aTree of
                  EmptyNode -> False
                  TriNode a left middle right -> if a == aVal
                                                then
                                                  True
                                                else
                                                  (inTree aVal left) ||
                                                  (inTree aVal middle) ||
                                                  (inTree aVal right)


-- | leafList: Finds values of all leafNodes
-- @aTree: the tree to find the leafs of
-- @return: a list of leaf node values
leafList aTree = case aTree of
                EmptyNode -> []
                TriNode a left middle right ->
                                              if (left == EmptyNode) && (middle == EmptyNode) && (right == EmptyNode)
                                                then [a]
                                              else (leafList left) ++ (leafList middle) ++ (leafList right)






--folding:
allTrue [] = True
allTrue (x:xs) = if x == True
                then allTrue xs
                else False
-- type: allTrue :: [Boolean] -> Boolean


split :: [a] -> ([a],[b])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:xs) = let (a,b) = split xs
                  in
                    (x:a, y:b)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = (x:xs)
