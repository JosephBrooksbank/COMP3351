module W2 where


data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)


variablesToString patt = case patt of
                    VariablePat string -> [string]
                    ConstructorPat (string, patt) -> variablesToString patt
                    TuplePat (patt) -> foldl (\x y -> (variablesToString y) ++ x) [] patt
                    _ -> []


uniqueString list = if (elem (head list) (tail list)) then False
       else uniqueString (tail list)



checkPat patt = uniqueString (variablesToString patt)
