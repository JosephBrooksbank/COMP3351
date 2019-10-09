module W2 where


data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)


variablesToString patt = case patt of
                  WildcardPat -> "*"
                  VariablePat string -> string
                  -- UnitPat -> NOTE NOT SURE 
                  ConstantPat num -> show num
                  ConstructorPat ->


checkPat patt =
