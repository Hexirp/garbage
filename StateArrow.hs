module StateArrow where
    import Control.Arrow
    import Control.Monad.State

    -- 状態を持つ機械は値と状態を分離し、次のように表せる。
    -- Run s t = (s, t) -> (s, t)

    -- ここで、それらを組み合わせて新しい機械を作りたいとする。最も単純なのは並列実行である。
    -- combine :: Run a b -> Run c d -> Run (a, c) (b, d)

    --   +-- (a, c) --+
    -- --+            +-->
    --   +-- (b, d) --+

    -- ここで、Runを変形する。
    -- Run s t = (s, t) -> (s, t) | カリー化
    -- Run s t = s -> t -> (s, t) | Stateを使う
    -- Run s t = s -> State t s   | 変形終わり

    -- これはArrowのインスタンスであるKleisli m(mはMonad)と一致する。
    -- Kleisli m a b = Kleisli { runKleisli :: a -> m b }
    -- Run s t = Kleisli (State t) s s

    -- よって、RunはArrowである。
    -- また、combineは(|||)である。

    pl :: Kleisli (State Int) Int Int
    pl = Kleisli $ \s -> state $ \t -> (s + t, t)

    ml :: Kleisli (State Int) Int Int
    ml = Kleisli $ \s -> state $ \t -> (s * t, t)

    pm :: Kleisli (State Int) (Int, Int) (Int, Int)
    pm = pl *** ml

    -- combineでは表現できるものがとても少ない。

    -- たとえば、二つの状態ありラインとがあるとして、Boolによりどちらかを実行し、もう一方では何もしないと言う組み合わせがほしいとする。