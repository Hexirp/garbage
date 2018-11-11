{-# LANGUAGE NoImplicitPrelude #-}

-- | tested
module PackageDataContinuationInstanceFunctor where
    import PackageDataContinuation
    import PackageDataFunction
    import PackageDataTuple
    import PackageStructureCategoryFunctor

    instance Singletype (Cont r) where
        singletype = Cont $ \r -> -- r: 渡された関数
            r $ Unit              -- rにUnitを適用する。

    instance Functor (Cont r) where
        fmap f a = Cont $ \r ->    -- 継続ではないといけないので、コントストラクタで包む。 f: 適用したい関数, a: 継続モナドに包まれた値, r: 渡された関数.
                runCont a $ \a' -> -- aのコントストラクタを外す。aの内部には値の情報が含まれており、それを取り出すには関数を渡すしかないので、a'で値を受ける。 a':aの内部に入っていた値.
                    r $ f $ a'     -- a'にfを適用し、その結果にrを適用する。

    instance Apply (Cont r) where
        lift2 f a b = Cont $ \r -> -- 上でやっていたことの引数が二つになっただけである。
            runCont a $ \a' -> 
                runCont b $ \b' -> 
                    r $ f a' b'

    instance Bind (Cont r) where
        join a = Cont $ \f ->        -- 継続ではないといけないので、コントストラクタで包む。 a: 継続モナドに二重に包まれた値, f: 渡された関数.
            runCont a $ \a' ->       -- a': 継続モナドに包まれた値.
                runCont a' $ \a'' -> -- a'': aの内部に入っていた値.
                    f a''            -- a''にfを適用する。