-- expressions stuff I guess




data Expr Int = Number Int |
                      Plus Expr Expr |
                      Subtract Expr Expr |
                      Mult Expr Expr |
                      Divide Expr Expr |
                      Mod Expr Expr
                      deriving(Show)


eval e env = case e of
        Number n -> applyEnv name env
        Plus a b -> a + b
        Subtract a - b
        Mult a b -> a * b
        Divide a b -> a / b
        Mod a b -> a mod b


data Env = EmptyEnv | Entry String Int Env
  deriving(Show)

applyEnv name EmptyEnv = error "Unbound variable"
applyEnv name (Entry n value e) = if name == n
  then value
  else applyEnv name e



extendEnv name value e = Entry name value e
