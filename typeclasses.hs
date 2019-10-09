data AntState a = Sleeping |Eating |Foraging |Carrying

instance (Eq a) => Eq (AntState a) where
  Sleeping == Sleeping = True
  Eating == Eating = True
  FOraging == Foraging = True
  Carrying x == Carrying y = x == y
  _ == _ = False

instance Show (AntState a) where
  show Sleeping = "Don't Bother Me!"
  show Eating = "Can't Talk. Eating..."
  show Foraging = "Looking for food"



-- functor example
-- instance Functor [] where fmap = map

data Tree a = EmptyNode | TreeNode a (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap f EmptyNode = EmptyNode
  fmap f (TreeNode val left right) = TreeNode (f val) (fmap f left) (fmap f right)





-- Getting input

main = do
  putStr "Enter your name"
  name <- getLine
  putStrLn ("Hello" ++ name)
