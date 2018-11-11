module CalculationModel where
    data Turing state alphabet = Turing (TuringState state alphabet) (TuringFunction state alphabet)

    data TuringState state alphabet = TuringState (TuringTape alphabet) (TuringMiniState state)

    data TuringTape alphabet = TuringTape (TuringInfList (TuringAlphabet alphabet)) (TuringAlphabet alphabet) (TuringInfList (TuringAlphabet alphabet))

    data TuringInfList a = TuringCon a (TuringInfList a) | TuringRepeat a

    data TuringMiniState state = TuringStart | TuringMiniState state | TuringEnd

    data TuringAlphabet alphabet = TuringBlank | TuringAlphabet alphabet

    data TuringFunction state alphabet = TuringFunction ((TuringMiniState state, TuringAlphabet alphabet) -> (TuringMiniState state, TuringAlphabet alphabet, TuringWay))

    data TuringWay = TuringLeft | TuringRight

    step :: Turing s a -> Turing s a
    step turing = 
        let 
            Turing state func = turing
            TuringState tape st = state
            TuringTape left top right = tape
            TuringFunction f = func
            (ns, na, nw) = f (st, top)
        in 
            case nw of
                TuringLeft -> Turing (TuringState (leftMove $ TuringTape left na right) ns) func
                TuringRight -> Turing (TuringState (rightMove $ TuringTape left na right) ns) func

    leftMove :: TuringTape a -> TuringTape a
    leftMove (TuringTape left top right) = case left of
        TuringCon a as -> TuringTape as a (TuringCon top right)
        TuringRepeat a -> TuringTape (TuringRepeat a) a (TuringCon top right)

    rightMove :: TuringTape a -> TuringTape a
    rightMove (TuringTape left top right) = case right of
        TuringCon a as -> TuringTape (TuringCon top left) a as
        TuringRepeat a -> TuringTape (TuringCon top left) a (TuringRepeat a)

    --data Lambda = Tarm Var | Lambda Var Lambda | Apply Lambda Lambda

    --type Var = String

    --calc :: Lambda -> Lambda
    --calc l = 
    --    case l of
    --        Tarm v -> l
    --        Lambda v l' -> Lambda v (calc l')
    --        Apply a b -> case a of
    --            Tarm v -> l
    --            Lambda v l' -> calc $ replace b v l'
    --            Apply c d -> calc $ Apply (calc a) b

    --replace :: Lambda -> Var -> Lambda -> Lambda
    --replace a v b = 
    --    case b of 
    --        Tarm w -> if w == v 
    --            then Tarm b
    --            else Tarm w
    --        Lambda w c -> if w == v 
    --            then Lambda w c
    --            else Lambda w (replace a v c)
    --        Apply c d -> Apply (replace a v c) (replace a v b)