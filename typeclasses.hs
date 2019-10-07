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



data Tree a = EmptyNode | TreeNode a (Tree a) (Tree a)
