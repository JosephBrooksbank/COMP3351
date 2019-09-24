
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


-- | inOrderMap: takes tree and applies function to it
-- @aTree: The tree to "modify" (use as base)
-- @function: The function to apply to every value in the tree
-- @return a new tree with values given by the original tree put through the function
inOrderMap function aTree = case aTree of
                            EmptyNode -> EmptyNode
                            TriNode a left middle right ->
                                  TriNode (function a) (inOrderMap function left) (inOrderMap function middle) (inOrderMap function right)



-- | preOrderFold: Applies function to every value in tree and folds tree
-- @function: The function to apply to each value
-- @val: the starting value to combine with the tree values
-- @aTree: the tree to fold
-- @return: A tree folded with @function, (usually an int)
preOrderFold function val aTree = case aTree of
                                  EmptyNode -> 0
                                  TriNode a left middle right ->
                                    (function val a) + (preOrderFold function 0 left)
                                                   + (preOrderFold function 0 middle)
                                                   + (preOrderFold function 0 right)






--folding:
allTrue [] = True
allTrue (x:xs) = if x == True
                then allTrue xs
                else False
-- type: allTrue :: [Boolean] -> Boolean


-- split :: [a] -> ([a],[b])
-- split [] = ([],[])
-- split [x] = ([x],[])
-- split (x:y:xs) = let (a,b) = split xs
--                   in
--                     (x:a, y:b)
--
-- merge :: (Ord a) => [a] -> [a] -> [a]
-- merge [] [] = []
-- merge (x:xs) [] = (x:xs)
