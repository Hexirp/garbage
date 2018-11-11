module NewLanguage where
    data Module = Module Name [First]

    type Name = String

    data First = Import Name WrapName ImportList | Function Name Expr | Type Name | Constractor Name | Def Expr | Infix Way Power [Operator]

    type WrapName = Name

    type Way = Bool

    type Power = Int

    data ImportList = ImportList WhiteBlack [Name]

    type WhiteBlack = Bool

    data Expr = Tarm Name | Lambda Name Expr | Apply Expr Expr | In Expr Expr | Forall Name Expr | Exist Name Expr

    type Operator = Name

    expr = Module "test" [Function "apply" $ Expr $ Lambda "f" $ Lambda "a" $ Apply (Tarm "f") $ Tarm "a", Def $ Exist "a" $ Exist "b" $ In (Tarm "apply") $ Apply "->" $ Apply "a" "b"]