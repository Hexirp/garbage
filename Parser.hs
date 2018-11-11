{-# LANGUAGE NoImplicitPrelude #-}

module Parser where
    import Prelude hiding (any, all, repeat, head)

    data Parsing a = Failure a | Success a

    instance (Show a) => Show (Parsing a) where
        show (Failure a) = "PARSING FAILED: " ++ show a
        show (Success a) = "PARSING SUCCEEDED: " ++ show a

    type Parser = String -> Parsing String

    char :: Char -> Parser
    char a [] = Failure []
    char a x@(xr:xs) = if a == xr 
        then Success xs 
        else Failure x

    any :: Parser
    any [] = Failure []
    any (xr:xs) = Success xs

    string :: String -> Parser
    string [] = Success
    string (x:xs) = char x >>> string xs

    all :: Parser
    all = repeat any

    end :: Parser
    end = nonhead $ any >>> all

    (>>>) :: Parser -> Parser -> Parser
    f >>> g = \x -> case f x of
        Failure a -> Failure a
        Success a -> g a

    infixl 1 >>>

    (<|>) :: Parser -> Parser -> Parser
    f <|> g = \x -> case f x of
        Failure a -> g a
        Success a -> Success a

    infixl 2 <|>

    repeat :: Parser -> Parser
    repeat f = \x -> case f x of
        Failure a -> Success a
        Success a -> repeat f a

    repeat' :: Parser -> Parser
    repeat' f = f >>> repeat f

    option :: Parser -> Parser
    option f = \x -> case f x of
        Failure a -> Success a
        Success a -> Success a

    head :: Parser -> Parser
    head f = \x -> case f x of
        Failure a -> Failure a
        Success a -> Success x

    nonhead :: Parser -> Parser
    nonhead f = \x -> case f x of
        Failure a -> Success a
        Success a -> Failure x

    peg :: Parser
    peg = repeat (line >>> char ' ') >>> option line >>> end
        where 
            line = name >>> string " = " >>> expr >>> string ";"
            name = repeat' nonbrank
            nonbrank = nonhead (char ' ') >>> nonhead (char '=') >>> nonhead (char ';') >>> nonhead (char '/') >>> nonhead (char '(') >>> nonhead (char ')') >>> nonhead (char '*') >>> nonhead (char '+') >>> nonhead (char '?') >>> nonhead (char '&') >>> nonhead (char '!') >>> any
            expr = conect <|> rep <|> rep2 <|> opt <|> hd <|> nh <|> name
            braket = char '(' >>> expr >>> char ')'
            value = braket <|> name
            conect = value >>> char ' ' >>> value
            rep = value >>> char '*'
            rep2 = value >>> char '+'
            opt = value >>> char '?'
            hd = char '&' >>> value
            nh = char '!' >>> value

    main :: IO ()
    main = do 
        putStrLn "--"
        a <- return $ char 'a'
        print $ a "abc"
        print $ a "bac"
        print $ a ""
        putStrLn "--"
        ab <- return $ a >>> char 'b'
        print $ ab "abc"
        print $ ab "acb"
        print $ ab "bca"
        putStrLn "--"
        aob <- return $ a <|> char 'b'
        print $ aob "abc"
        print $ aob "bca"
        print $ aob "cab"
        putStrLn "--"
        ar <- return $ repeat a
        print $ ar "aaa"
        print $ (ar >>> a) "aaaaaa"
        print $ ar "aab"
        putStrLn "--"
        ao <- return $ option a
        print $ ao "aa"
        print $ ao ""
        print $ ao "bc"
        putStrLn "--"
        ha <- return $ head a
        print $ ha "abc"
        print $ ha "bac"
        print $ ha "aaa"
        putStrLn "--"