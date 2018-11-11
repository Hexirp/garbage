{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageDataBool where
    import Text.Show (Show)

    data Bool = False | True deriving (Show)

    (&&) :: Bool -> Bool -> Bool
    a && b = case a of
        False -> False
        True -> b

    (||) :: Bool -> Bool -> Bool
    a || b = case a of
        True -> True
        False -> b

    (&>) :: Bool -> Bool -> Bool
    a &> b = case a of
        True -> False
        False -> b

    (|>) :: Bool -> Bool -> Bool
    a |> b = case a of
        False -> True
        True -> b

    not :: Bool -> Bool
    not False = True
    not True = False

    otherwise :: Bool
    otherwise = True

    bool :: Bool -> a -> a -> a
    bool False a b = b
    bool True a b = a