{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

{-
    MultiParamTypeClassesは型クラスに関する言語拡張でり、複数の引数を取る型クラスを定義できる。
    class Tran t op where ... を書くために使っている。

    FlexibleContextsは上と同じ言語拡張であり、型パラメータを複数持つ型クラスに関する制約が書ける。
    class Tran t () => Sim t where　... を書くために使っている。
-}

module Game where 
    -- Tran = Transition
    class Tran t op where 
        -- tがTransition(変化する物)であることは、

        -- op(操作)により定まる 「tからtへの変換」によって更新できることである。
        next :: op -> t -> t

    -- Sim = Simulation
    class Tran t () => Sim t where
        --　tがSimulation(シミュレーション)であることは、opが()となっているTransitionであることである。つまり、操作できないTransitionである。

        -- これは状態を一つ進める関数を持つ。
        step :: t -> t
        step t = next () t

    class Tran t op => Game t op where
        -- tがGameであることは、tがTranのインスタンスであり、

        -- tの状態を画面に表示でき、
        render :: t -> IO ()

        -- ini = initial = 初期 -- 一定の初期状態があるということである。
        ini :: t

    -- RTGame = RealTimeGame
    class Game t op => RTGame t op where
        -- tがRTGame(リアルタイムゲーム)であることは、tとopがGameであり、

        -- opを外部の入力から持ってくる関数が存在するということである。
        input :: IO op

        -- adTime = advanceTheTime = 時間を進める -- これはIO化されたnextを持つ。
        adTime :: IO op -> IO t -> IO t
        adTime op t = next <$> op <*> t

        -- これは状態を外部からの入力によって更新する関数を持つ。
        realTime :: IO t -> IO t
        realTime = adTime input

{-
    トラブルシューティング。5/5。adTimeを定義しようとしたらできなかった。悪い定義。

    class Game t op => RTGame t op where
        -- tがRTGame(リアルタイムゲーム)であることは、tとopがGameであり、

        -- opを外部の入力から持ってくる関数が存在するということである。
        input :: IO op

        -- adTime = advanceTheTime = 時間を進める -- これはIO化されたnextを持つ。
        adTime :: IO op -> IO t -> IO t
        adTime op t = next <$> op <*> t

        -- これは状態を外部からの入力によって更新する関数を持つ。
        realTime :: IO t -> IO t
        realTime = adTime input

    realTimeで問題が発生。

    問題点は分かった。input :: IO opとか書いてあるが、本当はRTGame t(in) op(in) => IO op(in)。
    同様に、adTime :: IO op -> IO t -> IO tは、RTGame t(ad) op(ad) => IO op(ad) -> IO t(ad) -> IO t(ad)

    さらに、RTGame　t opのtとopは独立で、それぞれ自由に選べる。これを合わせて型推論の過程を再現する。

    adTime input :: (RTGame t(ad) op(ad) => IO op(ad) -> IO t(ad) -> IO t(ad)) $ (RTGame t(in) op(in) => IO op(in))
                 :: (RTGame t(ad) op(ad+in) , RTGame t(in) op(ad+in)) => (IO op(ad+in) -> IO t(ad) -> IO t(ad)) $ (IO op(ad+in))
                 :: (RTGame t(ad) op(ad+in) , RTGame t(in) op(ad+in)) => (IO t(ad) -> IO t(ad))

    これに対して、realTimeの型。

    realTime     :: RTGame t(rt) op(rt) => IO t(rt) -> IO t(rt)

    ひどい。t(ad) = t(in)が示すことができれば。
-}

{-
    -- Phase = 区切り -- いくつかの区切りがあるゲーム。戦闘、移動、ミニゲーム、プレイヤーの手番など。
    class Game t op => PhaseGame t op where
        -- tがPhaseGameであることは、

        -- Phaseの情報を取り出す関数があることである。
        phase :: t -> t

    -- SeqGame = Sequential game = 同時手番ゲーム -- プレイヤーの行動が同時に行われるゲーム
-}