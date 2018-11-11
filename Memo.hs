module Memo where
	{- 
		Monadは、箱のような性質を持った型の「総称」である。
		決してただ一つの型を表しているのではない。
		さらに、returnや>>=などの関数の名前も本来は何でもよい。
		これらの名前が決められている理由は単純に、何がモナドか分かりやすくするためだ。
		ただの型だと思っていたらモナドでした、なんてことが起こったら、プログラマーは間違いなくいら立ちを感じるだろう。

		さて、Monadははこのような性質を持っていると言った。
		しかし、箱の中に入った物を取り出すことはできない。なぜか。
		それは、Monadは単なる箱ではなく、特別な箱であるからだ。
		まずは、Monadクラスを自分で定義し、ただの箱であるモナドを考えてみよう。
	-}

	{- 
		class Monad m where
			-- この関数はデフォルトでは return という名前であるが、この名前のほうが他のプログラミング言語を使用している人たちにとって分かりやすいので、変更した。
			pure :: a -> m a

			infixl 1 >>=
			(>>=) :: m a -> (a -> m b) -> m b

			-- この関数の必要性は分からないと思うが、入出力に関する範囲で使われる。後々解説するので待ってほしい。
			infixl 1 >>
			(>>) :: m a -> m b -> m b
			x >> y = x >>= (\_ -> y)

			-- ここから下の関数はどういう関数なのかすぐ分かると思うから、解説は書かない。
			infixr 1 <<=
			(<<=) :: (a -> m b) -> m a -> m b
			x <<= f = f >>= x

			infixr 1 <<
			(<<) :: m b -> m a -> m b
			x << y = y >> x
	-}

	{-
		これがモナドの定義である。しかし、型だけの定義が多く、その意味ははっきり言って分からない。
		この意味という物を解説していきたいと思う。

		ちなみに、本当のMonadは実はもう一つ関数が定義されているのだが、
		その関数は普通の関数と呼ぶにはいささか特殊である入出力に関係する関数であるため、省略した。
		この関数の定義はこれである。

		fail :: String -> m a
		fail msg = error msg

		型自体は何でもないのだが、デフォルトの定義の時点で普通の関数ではないと分かるだろう。
		これは後々解説する。しかし、単純に言うと、これは計算が失敗した時に「Haskellの処理系」に呼ばれる関数である。
		つまり、処理系が絡んでいる特殊な関数である。

		話を戻すと、ただの箱であるMonadはどのような物であるかと考えると、
		何かの型を入れられる最も単純な型である次の型が考えられる。
	-}

	-- Id は恒等という意味で使われる。関数のidをイメージしていただけると分かりやすいだろう。
	data Id a = Id a deriving (Show)

	-- 後で解説するが、これはIdだけで出来ることである。
	down :: Id a -> a
	down (Id x) = x

	{-
		これをMonadのインスタンスにする。しかし、まずはApplicativeのインスタンスになる必要がある。
		さらにApplicativeのインスタンスになるにはFunctorのインスタンスになる必要がある。

		なぜ、こんなめんどくさい仕様になったのかというと、実は前はMonadのインスタンスになる時に特に別のインスタンスになる必要はなかったのだ。
		しかし、MonadはApplicativeの特殊なバージョン、ApplicativeはFunctorの特殊なバージョンだということが分かってきたのだ。

		すると、ちょうどオブジェクト指向で継承を使いコードを使いまわすように、Monadのコードもさらに一般的なクラスのコードを使いまわしたほうがいいのではないかと考えられ、
		それができるようにMonadはApplicativeを継承(Haskellの言葉。オブジェクト指向とは違う。)するようになったのだ。

		そこで、まずはFunctorのインスタンスにする。
	-}

	instance Functor Id where
		-- 型は　(a -> b) -> f a -> f b で、fはクラスのインスタンスになる型だから、 (a -> b) -> Id a -> Id b
		fmap f (Id x) = Id $ f x

	{-
		さて、これでFunctorのインスタンスにできた。しかし、Functor則という法則を満たす必要がある。これは次のような法則である。
		===は両方の文が同じ意味であることを表している。

		1. fmap id  ===  id
		2. fmap (f . g)  ===  fmap f . fmap g

		この法則の意味は、一番目はidがそのまま意味が変わらないことを示す。つまり、fmapを適用された関数の意味が大きく変わることがないことを示す。
		二番目は一番目と同様、意味が変わらないことを合成した関数に対して示している。
		これを合わせて、fmapが何か変なことをしてはいけないことを示す。
	-}

	instance Applicative Id where
		-- この関数の型は a -> Id a になる。
		pure = Id

		-- この関数の型は Id (a -> b) -> Id a -> Id b である。
		Id f <*> Id x = Id $ f x

	{-
		Applicativeは本来の名前はApplicativeFunctorであり、意味は適用できるFunctorである。
		意味がよくわからないと思うが、<*>の型を見てほしい。fmapが　(a -> b) -> m a -> m b であるのに対して、
		<*>は　m (a -> b) -> m a -> m b であり、fmapは普通の関数がモナドに入った値へ適用されているのに対して、
		<*>はモナドに入った関数と値同士で適用されている。

		つまり、これは「モナドに入った物同士でも計算できるよ！」ということを表す型である。

		これにもApplicative則というめんどくさいものがある。

		1.pure id <*> v = v
		2.pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
		3.pure f <*> pure x = pure (f x)
		4.u <*> pure y = pure ($ y) <*> u

		これらをすべて確認するのはめんどくさいので、この定義した関数を何回か使ってみて、あっているなと思ったらOKである。
		なにかおかしいと感じたらきちんと確認することが大事である。
	-}

	instance Monad Id where
		-- この関数の型は Id a -> (a -> Id b) -> Id b になる。
		Id x >>= f　= f x

	{-
		これでモナドの実装は終わった。簡単だと思われるかもしれない。しかし、そうではない。
		前に書いたが、Monadには型だけの定義が多い。しかし、これらの定義はあるルールによって縛られているのだ。
		知ってるかもしれないが、それをMonad則という。

		1. pure x >>= f             ===  f x
		2. x >>= pure               ===  x
		3. x >>= (\a -> f a >>= g)  ===  x >>= f >>= g

		これは理解するのは困難である。なので、はっきりと書いていない所をはっきりさせる。

		-- M はMonadである。
		1. pure x >>= f               ===  f x
		2. M x >>= pure               ===  M x
		3. M x >>= (\a -> f a >>= g)  ===  M x >>= f >>= g

		この法則の意味するのは何なのか。第一法則は、xにpureを適用したはずなのに、それを>>=に通すと消えている。
		次の第二法則も同じようなもので、xにpureを適用したはずなのに、何も変わっていない。この二つの法則は似ている。
		この二つの法則はpureに「何も意味がない」ということを求めている。ちょうど、関数のidと同じような感じである。
		たとえば、pureに2を適用したら、3が出てきたということはあってはならないということである。

		第三法則は、難しいので、さらに簡略化する。
		(\a -> f a >>= g)　という部分はさらに単純にできないだろうか?
		そのための関数を作ろうと思う。
	-}

	{-
		infixl 1 >=>
		(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
		f >=> g = (\x -> f x >>= g)

		infixr 1 <=<
		(<=<) :: (b -> m c) -> (a -> m b) -> (a -> m c)
		f <=< g = g >=> f
	-}

	{-
		これは>>=の二番目の引数になっている　a -> m b という型を持った関数を引数なしにつなげるための関数でもあり、
		第三法則に現れる形と同じ形になっている関数である。これを使うと第三法則はこのようになる。

		3. m x >>= (f >=> g)  ===  m x >>= f >>= g

		ところで、>>=を反転した<<=という関数の型と結合の順番、何か$に似ていないだろうか?

		($)   :: (a -> b)   -> a   -> b
		(<<=) :: (a -> m b) -> m a -> m b   -- m はMonadである。

		a $ b $ c     = a $ (b $ c)
		a <<= b <<= c = a <<= (b <<= c)

		この二つの関数は深い関係を持っているのである。さて>>=を反転した<<=が普通の関数と関係を持っているのなら、
		>=>を反転した<=<も何かと関係を持っているのか？この関数の型から m を取り除いてみよう。すると何か見たことのあるような形になった。
		.(関数を合成する関数)の型と同じである。結合の順番も同じである。

		(.) :: (b -> c) -> (a -> b) -> (a -> c)
		(<=<) :: (b -> m c) -> (a -> m b) -> (a -> m c)   -- m はMonadである。

		f . g . h     = h . (g . h)
		f <=< g <=< h = f <=< (g <=< h)

		反転した関数を使うと、第三法則はこうなる。

		3. (g <=< f) <<= m x  ===  g <<= f <<= m x   -- m はMonadである。

		さらに、色々変形していくとこうなる。

		3. (f <=< g) <=< h  ==  f <=< (g <=< h)

		ここから、第三法則の意味が分かる。
		関数を適用する順序で意味が変わったりすることは駄目だということである。ここでの順序が変わるということは、
		(f . g) . h  ->  f . (g . h) ということで、 h . g  ->  g . h ではない。
		これは結合法則といい、足し算はこれが成り立つ。ちなみに、両側が入れ替わるのは交換法則といい、これも足し算は成り立つ。
		両方とも成り立たないものとして、引き算がある。また、関数の合成は結合法則は成り立つが、交換法則は成り立たない。興味を持った人は調べてほしい。

		さて、Idがこの法則を満たすことを確認しよう。

		1. pure x >>= f = Id x >>= f
		                = f x

		2. Id x >>= pure = pure x
		                 = Id x

		3. Id x >>= (\a -> f a >>= g) = (\a -> f a >>= g) x
		                              = f x >>= g

		   Id x >>= f >>= g = f x >>= g

		これは簡単に確認できた。しかし、様々なMonadがこれから出てきて、それらすべてを確認するのは難しい。
		そのため、確認は書かない。どうしてもしたいなら自分でしてほしい。

		さて、Idモナドを作ったが、もっとほかのMonadはないのだろうか？ある。それは単純な箱ではなくもっと複雑だ。
		ここから、Monadが考え出されるまでの過程を疑似的に体験してほしい。

		あなたはHaskellで他のプログラミング言語にあるnullのようなものを作れないか悩んでいた。
		すると、それを型で作るアイデアが天から降ってきた。
	-}

	{-
		-- Nothing は　null や失敗したことを表す。　Just は成功したことを表し、その結果が入っている。
		data Maybe a = Nothing | Just a
	-}

	-- 試しにこれを使ってエラーが出ない安全な割り算を作ってみよう。

	infixl 7 ./.
	(./.) :: Integral a => a -> a -> Maybe a
	x ./. 0 = Nothing
	x ./. y = Just (x `div` y)

	{-
		しかし、返された値はどうするのか？中身を後で計算に使いたいので、Maybeに入っている物を取り出したい。
		例としてこのようなものが考えられる。

		pickOut :: Maybe a -> a
		pickOut (Just x) = x

		しかし、これではエラーになる。Nothingが考慮されていないからだ。しかし、Nothingの中身の値とは？
		ないので、このような関数を作るのは無理である。なので発想を逆転して、Maybeに入っている値に関数を適用してやる関数を作ってみよう。
		これは作用が$に似ているので、名前は$$とする。
	-}

	infixr 0 $$
	($$) :: (a -> b) -> Maybe a -> Maybe b
	f $$ x = case x of
		Nothing -> Nothing
		Just a -> Just $ f a

	{-
		これでMaybeに入った値に関数を適用できるようになった。値がJustだった場合は中身に関数を適用し、Justに入れる。
		Nothingだった場合は値がないので、Nothingのままである。しかし、この関数の型は何かに似ていないだろうか？
		実はfmapとまったく同じである。ここから、fmapの存在理由、つまり目的が読み取れる。

		-- fmapはモナドの中の値に関数をモナドの性質を考慮して適用するための関数である。

		ならば、<*>や>>=にもそれなりの存在意義があるはずである。
		
		-- <*>はモナドに関数を部分適用した結果であるモナドの中に入った関数を、モナドの中に入った値に適用するための関数である。

		たとえば、割り算をfmapを使って部分適用するとしよう。コードはこうなる。

		fmap div (Just 4)

		このコードの結果は、 Just (\x -> div 4 x) である。さて、ここにモナドに入った関数が出てきた。
		これをモナドの中に入った値に適用したい。コードはこのようになる。

		fmap div (Just 4) <*> (Just 2) -- 結果: Just 2

		<*>の存在理由が分かっただろうか？さらに、fmapが中置関数になった<$>を使うとこう書ける。

		div <$> Just 4 <*> Just 2 -- 結果: Just 2

		美しいコードである。途中に例外を入れて見よう。

		div <$> Just 4 <*> Nothing -- 結果: Nothing

		例外に関数を適用したら例外になるのは当たり前である。このような型の意味をきちんと考慮して、fmapや<*>などの関数を実装しなければならない。
		Maybeの意味は「例外であるかもしれない値」である。この型につけられた意味を難しくいうと文脈という。

		では、>>=を意味を考慮して実装してみよう。

		x >>= f = case x of
			Nothing -> Nothing
			Just a -> f a

		これでできた。この関数の意味は、返り値が自動的にJustになるのではなく、
		関数が自分自身で成功したかどうかを決められるようになったfmapである。

		ここでMaybeでの関数の実装をまとめる。

		fmap f x = case x of
			Nothing -> Nothing
			Just a -> Just $ f a

		pure = Just

		f <*> x = case f of
			Nothing -> Nothing
			Just g -> case x of
				Nothing -> Nothing
				Just a -> Just (g a)

		x >>= f = case x of
			Nothing -> Nothing
			Just a -> f a

		ここで疑問が出てくる。前に作ったIdという型の文脈は何だろうか？
		それは何もない。つまり、何も意味がない文脈だということだ。
		これはIdの定義を見ても分かる。Idで出来ることは普通にやってもできることが分かると思う。

		さて、Maybeは例外かもしれない文脈を表していた。他の文脈があるだろうか？
		その一つは可能性である。たとえば、Aかもしれないし、Bかもしれないという値を作りたいと思ったらどうすればいいのだろうか？
		
		実はそれはリストで表されるのである。リストは同じ型のデータをまとめるという意味を持っているのであるが、
		もう一つ、様々な可能性を表すモナドでもあるのである。
		上のAかもしれないし、Bかもしれないという値は [a,b] と書かれる。
		最も単純なリストは []　である。これは何を表すのだろうか？
		これは何も可能性がない。つまり、失敗を表すと言えるのではないのか？
		また、単純に成功したことを表すには可能性が一つだけある状態である [a] と書けばよい。

		ここから、リストはMaybeよりも色々な記述ができることが分かる。では、これらを実際にモナドのインスタンスにしてみよう。
		fmapはどうすればいいのだろうか？例を出して考えてみよう。[2]に(+2)を適用したら、[4]になるべきだ。
		また、[]に(+2)を適用したら[]になるべきだ。[3,2]に(+2)を適用したらどうなるのだろうか？
		リストは可能性を表すのであった。すると、この計算の結果は3に(+2)を適用したものかもしれないし、2に(+2)を適用したものかもしれないと考えることができる。
		つまり、この計算の結果は [3+2,2+2] -> [5,4] となると考えたほうが自然だろう。
		さて、これは何かに似ていないだろうか？mapである。名前が似ているのは偶然かどうかわからないが、fmapはmapと同じということで良さそうだ。

		fmap f x = case x of
			[] -> []
			(x : xList) -> f x : fmap f xList
		
		次にpureを考える。これは単純に可能性が一つだけの形にすればいい。
		pure x = [x]

		次に<*>を考える。たとえば、[2,3]に[+4,+1]を適用したらどうなるのだろうか？
		リストは可能性を表すんだった。全ての可能性を書き出してみよう。
		2と+4、2と+1、3と+4、3と+1。これらがすべての可能性である。
		これをすべて含むリストは[2+4,2+1,3+4,3+1]である。
		リストはすべての可能性を保存するからこれは自然だろう。

		f <*> x = case f of
			[] -> []
			f : fList -> case x of
				[] -> []
				xs@(x : xList) -> (f <$> xs) ++ (fList <*> xs)

		次に>>=だ。これはリストを返す関数をリストにfmapした後でくっ付ける。

		x >>= f = case x of
			[] -> []
			(x : xList) -> f x ++ (xList >>= f)

		イメージとしては次のようになる。

		1. [3,5,6]
		2. [[3,-3],[5,-5],[6,-6]]
		3. [3,-3,5,-5,6,-6]

		実際、>>=は次のように書くこともできる。

		x >>= f = join $ f <$> x
			where 
				join [] = []
				join (x : xList) = x ++ join xList

		これでリストをモナドにできた。その他のモナドはたくさんあるから、よく使う物だけ紹介する。
		デフォルトのモナドの一覧はhttps://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Monad.htmlを見るといいだろう。
	-}

	{-
		さて、あなたは失敗をモナドで表せるようになった。しかし、今度は例外のメッセージを作りたくなったとしよう。これはMaybeをちょっといじれば出来る。

		data Either a b = Left a | Right b

		fmap f x = case x of
			Left a = Left a
			Right b = Right $ f b

		pure = Right

		f <*> x = case f of
			Left g = Left a
			Right h = case x of
				Left a = Left a
				Right b = Right $ h b

		x >>= f = case x of 
			Left a = Left a
			Right b = f b

		これで作ることができた。つまり、EitherはNothingに値を入れられるようになったMaybeということである。
	-}

	{-

		関数もモナドである。なぜ関数がモナドになれるのか？それは関数もある意味を持っていると考えることができるからである。
		それは「何かを適用しないと値が出てこない」という文脈である。実際に例を示そう。

		まずfmapはどういう関数になるか考えよう。型は (a -> b) -> (r -> a) -> (r -> b) である。
		さて、これはどういう物になるのだろうか？関数は「何かを適用しないと値が出てこない」ということを表すんだった。
		fmapは最初に渡された関数を二番目の引数に適用する関数だった。ならば、関数モナドでは何に第一引数を適用すればいいのだろうか？
		自然なのは「第二引数から出てくるはずの値」である。つまり、関数のfmapはこのような関数だと考えられる。

		fmap f g = \x -> f (g x)

		これはgから出てくるはずの値にfを適用している。関数のfmapの返り値は関数になっているから、これも関数になっている。
		さて、今まで触れてきなかったが、関数のfmapは.の型と同じである。さらに定義も.と同じである。
		つまり、こう書き換えられるということである。

		fmap f g = f . g

		単純になった。これからも関係ないように見えるが、他の関数と実は同じだったということが時々出てくる。

		次にpureである。この関数の型は a -> (r -> a) である。さて、pureはモナドに好きな値を入れるための関数である。
		関数モナドでは関数に値を入れる関数になる。しかし、「関数に値を入れる」ということはどういうことなのだろうか？
		関数モナドは何か適用しないと値が出てこないことを表すんだった。これはちょうど自動販売機みたいである。
		自動販売機みたいな関数は何かと考えると、 \_ -> x という関数が考えられる。
		これは値がそのまま中に入っているにも関わらず、何かを適用しないと値が出てこない。これで良さそうだ。

		pure x = \_ -> x

		次に<*>である。この関数の型は (r -> (a -> b)) -> (r -> a) -> (r -> b) である。
		<*>はさっき言ったように、fmapで関数の部分適用をした結果出来る関数がモナドに入った物を、同じくモナドに入った関数に適用するための関数である。
		例を出して考えてみよう。 (*) <$> pure 3 <*> pure 4 の結果は \_ -> 12 になるはずである。

		1. (*) <$> pure 3
		2. (*) <$> (\_ -> 3)
		3. \x -> (*) ((\_ -> 3) x)
		4. \_ -> (*) 3

		つまり、　(\_ -> (*) 3) <*> pure 4 は \_ -> 12 になるはずである。途中まで計算してみる。

		1. (\_ -> (*) 3) <*> pure 4
		2. (\_ -> (*) 3) <*> \_ -> 4

		さて、これの返り値は \_ -> 12 になるべきである。　どうすればいいのか？まずは右側から値を取り出す必要がある。
		それには何かを適用すればいいから、この関数の返り値は関数になるべきだということを考えると、 \r -> (\_ -> (*) 3) r とすればよい。
		すると (*) 3 が返ってくるから、これを左側に適用すればよいと思いきや、こっちも値を取り出す必要がある。
		返り値は一つの引数を取る関数だから、もう一つの値を取ることはできないから、使いまわすしかない。
		すると、 \r -> (\_ -> (*) 3) r　$ (\_ -> 4) r となる。これを計算しよう。

		1. \r -> (\_ -> (*) 3) r　$ (\_ -> 4) r
		2. \_ -> (*) 3 $ 4
		3. \_ -> 12

		これで良さそうだ。さっき得られた式を一般化すると、こうなる。

		f <*> g = \x -> f x (g x)

		何とも奇妙な式となった。これが何の役に立つのか疑問に思ってしまう。pureを使わない式を見てみよう。

		(*) <$> (+ 3) <*> (+ 4)

		これを計算してみる。

		1. (*) <$> (+ 3) <*> (+ 4)
		2. (*) . (+ 3) <*> (+ 4)
		3. \x -> (((*) . (+ 3)) x) $ (+ 4) x
		4. \x -> ((*) $ (+ 3) x) $ (+ 4) x
		5. \x -> (*) ((+ 3) x) ((+ 4) x)
		6. \x -> (*) (x + 3) (x + 4)
		7. \x -> (x + 3) * (x + 4)

		これはどういう関数なのか？引数を二つに分けてそれぞれ関数を適用してから計算している。不思議な式である。
		関数を一般化してみよう。

		1. f <$> g <*> h
		7. \x -> f (g x) (h x)

		ここでfmapは関数を合成する働きをしていたことを思い出そう。つまり、<*>は複数の引数を取る関数の引数それぞれに関数を合成するための関数とみていいだろう。
		実際、fが三つの引数を取る場合を考えると、このようになる。

		1. f <$> g <*> h <*> i
		7. \x -> f (g x) (h x) (i x)

		普通の.ではできないことをするための関数が<*>だと言える。

		さて、最後である>>=を作ってみよう。型は (r -> a) -> (a -> (r -> b)) -> (r -> b) である。
		実装はこうである。最初に実装を書くわけは、この関数が<*>と大差ないからである。

		g >>= f = \x -> f (g x) x

		ここから分かるように、fの引数が入れ替わっているだけである。実際、>>=は<*>を使ってこのように書ける。

		g >>= f = flip f <*> g

		これは関数モナドの特徴の一つで、他のモナドではたぶんこんなことはできない。まとめよう。

		-- この型は　(a -> b) -> f a -> f b　で、fは (r -> _) だから、型は (a -> b) -> (r -> a) -> (r -> b) である。両方の引数が関数であることに注意。
		fmap f g = f . g

		-- この型は　a -> f a　で、fは (r -> _) だから、型は a -> (r -> a) である。
		pure x = \_ -> x

		-- この型は　f (a -> b) -> f a -> f b　で、fは (r -> _) だから、型は (r -> (a -> b)) -> (r -> a) -> (r -> b) である。最初の引数が二つの引数を取る関数であることに注意。
		f <*> g = \x -> f x (g x)

		-- この型は　f a -> (a -> f b) -> f b で、fは (r -> _) だから、型は (r -> a) -> (a -> (r -> b)) -> (r -> b) である。二つ目の引数が二つの引数を取る関数であることに注意。
		g >>= f = \x -> f (g x) x
	-}

	{-
		次に、状態を保持しながら計算したいとする。これは状態モナドを使えばできる。
		値に状態をつけるにはタプルを使えばいいから、型はこのようになる。

		newtype State s t = s -> (t, s)

		「おや？」と思うことがないだろうか？状態なのに関数になっている。これは状態の初期値が必要であるためである。

		さて、fmapを作ろう。型は (a -> b) -> (s -> (a, s)) -> (s -> (b, s)) である。
		これはモナドが中に持っている値に関数を適用すればいい。

		fmap f st = \s -> first f $ st s
			where first f (a, b) = (f a, b)
			
		簡単である。次にpureを作ろう。型は a -> (s -> (a, s)) である。これも簡単である。

		pure x = \s -> (x, s)

		次に<*>を作る。これは難しい。型は (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s)) である。
		もう一回いうが、これはfmapで関数の部分適用をした結果出来る関数がモナドに入った物を、同じくモナドに入った関数に適用するための関数である。

		関数モナドと同じことをしてみよう。 (*) <$> pure 3 <*> pure 4 は　\s -> (12, s)　になるべきである。
		
		1. (*) <$> pure 3 <*> pure 4
		2. (*) <$> (\s -> (3, s)) <*> (\s -> (4, s))
		3. (\s -> ((*) 3, s)) <*> (\s -> (4, s))

		さて、この後どうすれば自然になるだろうか？両方ともsを引数にとっている。これをまとめればいいのだろうか？やってみよう。

		f <*> st = \s -> (cut (f s) $ cut (st s), s)
			where cut (a, b) = a

		これでよいのだろうか？どうやらAplicative則も満たしていそうである。ところがどっこい、うまくいかないのである。
		原因は最初の関数を取り出す時にできた状態が使われていないからである。
		最初の例での状態モナドに入った関数は普通の物であるが、このようなものもあるかもしれないのである。

		1. (\s -> ((*) 3, [s])) <*> (\s -> (4, s))

		これを見ると、状態を切り捨ててはいけないと分かる。ではどうすればいいのだろうか？状態モナドは状態を持つ計算を表すはずだ。
		状態とは計算の中で変化しながら受け継がれていくものである。ならば、出来た状態を二番目の引数に使って値を取り出せばいいと分かる。

		これを考慮すると、こうなる。

		f <*> st = \s -> 
			let 
				(fun, fst) = f s
				(x, xst) = st fst
			in 
				(fun x, xst)

		この関数はまず、関数と状態を返すはずの第一引数fに、後で渡されるはずの初期状態sを適用し、それをletで分解している。
		出てきた関数はfun、出てきた状態はfstとなる。次に出てきた状態を、状態と値が出てくるはずの第二引数stに適用する。ここがポイントである。
		そして出てきた値をx、出てきた状態をxstとする。最後に、funにxを適用して、値とする。状態はxstのままである。

		複雑であるがこれを理解しないとつまずいてしまうので気を付けてほしい。

		さて、>>=を作る。型は (s -> (a, s)) -> (a -> (s -> (b, s))) -> (s -> (b, s)) である。
		これは簡単であるか？

		これは第一引数から値を取り出してそれを第二引数の関数に適用すれば良さそうだ。
テアー
		st >>= f = \s -> f (cut (st s))
			where cut (a, b) = a

		おっと、前と同じ道をたどっている。では状態を一度分けてみよう。

		st >>= f = \s ->
			let 
				(x, xst) = st s
			in 
				f x

		おかしいところがないだろうか？最後にfを使い状態を作ってもらう。問題はないはずなのだが。xstが使われていない。
		fの型をよく見てほしい。 (s -> (b, s))　となっている。これをそのまま返せばいいはずだ。
		しかしそうはできない。最初に書いた \s -> を見てほしい。これはカリー化により変形できる。

		(st >>= f) s = 
			let 
				(x, xst) = st s
			in 
				f x

		この場合の型は　 (s -> (a, s)) -> (a -> (s -> (b, s))) -> s -> (b, s) になる。おかしいところがはっきりした。
		この関数は (b, s)　を返さなければならないので、最後の関数の返り値は (s -> (b, s)) だから型が合わない。
		では型を合わせるにはどうすれば？ここでxstが出てくる。状態は連続するものなので、捨てているのがおかしいのである。これを使えばいい。

		(st >>= f) s = 
			let 
				(x, xst) = st s
			in 
				(f x)　xst

		カリー化を元に戻すとこうなり、これで完成である。

		st >>= f = \s ->
			let 
				(x, xst) = st s
			in 
				(f x)　xst
			
		-- この型は　(a -> b) -> f a -> f b　で、fは (s -> (_, s)) だから、型は (a -> b) -> (s -> (a, s)) -> (s -> (b, s)) である。
		-- 型関数を使うと (a -> b) -> State s a -> State s b
		fmap f st = \s ->
			let 
				(x, xst) = st s
			in
				(f x, xst)

		-- この型は　a -> f a　で、fは (s -> (_, s)) だから、型は a -> (s -> (a, s)) である。
		-- 型関数を使うと a -> State s a
		pure x = \s -> (x, s)

		-- この型は　f (a -> b) -> f a -> f b　で、fは (s -> (_, s)) だから、型は (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s)) である。
		-- 型関数を使うと State s (a -> b) -> State s a -> State s b
		f <*> st = \s -> 
			let 
				(fun, fst) = f s
				(x, xst) = st fst
			in 
				(fun x, xst)テアー
				
		-- この型は　f a -> (a -> f b) -> f b で、fは (s -> (_, s)) だから、型は (s -> (a, s)) -> (a -> (s -> (b, s))) -> (s -> (b, s)) である。
		-- 型関数を使うと State s a -> (a -> State s b) -> State s b
		st >>= f = \s ->
			let 
				(x, xst) = st s
				fst = f x
			in 
				fst xst
	-}

	{-
		次にIOモナドである。なぜIOモナドを最初に解説しないのかといえば、これは特殊なモナドであるからである。
		入力を受け取る、出力をするなどといった他のプログラミング言語では最も基本的な機能が、Haskellでは基本的ですらなく、複雑な部類であるのがこの言語の難しさの一因になっている。
		まずIOモナドはHaskellの内側で定義されているものではない。定義されている個所はHaskellのコンパイラなどの根源的なものである。

		しかしながら、例えを使って定義をするとこうなる。

		IO a = State RealWorld a

		これはどういうことなのか？IOモナドは現実の世界を状態とする状態モナドみたいなものなのである。
		結果がタプルになっていなかったりと、本当は違うのであるが、Haskellのコンパイラを自分で作るなどの超上級者ではないのであれば、違いが問題になることはない。

		たとえば、printの型は (Show a) => a -> IO () である。この関数はどういう物なのか。
		一つの文字列にできる物aを取り「現実世界を取り、文字列にされたaが表示された現実世界を返す関数」を返す関数である。現実世界を引数にする関数など、めちゃくちゃだが気にしないでいただきたい。

		「現実世界を取り、文字列にされたaが表示された現実世界を返す関数」を一般化すると、RealWorld -> RealWorld となる。

		さて、上で書いた現実世界を仮想世界と書き換えたらどうだろうか？とても自然である。つまり大げさに解釈すると、状態モナドは世界を作ってそこに状態を保存すするものだと言えるのではないのか？
		実のところ、ありとあらゆる計算は状態モナドだけを使って表現できる。状態モナドはモナドのボスのようなものである。

		Id a = State () a
		Either a b = State Bool (a,b)
		a -> b = State a b
		
		これらの関係は完全なものではない。しかし、少なくとも似たような機能を実現できる。

		さて、状態モナドを理解できたらIOモナドを理解できる。そのため、状態モナドの関数をもう一回詳しく解説する。

		fmapは状態モナドに入った値に関数を適用するための物である。
		この関数がすることは、いつか渡されるであろう初期状態sをstに適用して、値xと状態xstを取り出し、xにfを適用して、その結果とxstをまとめるということである。

		pureは状態モナドに入った値を作るための物である。
		この関数がすることは、xを取り初期状態sとまとめるということである。

		<*>は状態モナドに入った関数を状態モナドに入った値に適用するための物である。
		この関数がすることは、いつか渡されるであろう初期状態sをfに適用して、関数funと状態fstを取り出して、fstをstに適用して、値xと新しい状態xstを取り出して、funをxに適用して、その結果とxstをまとめるということである。

		>>=は状態モナドを返す関数を状態モナドに入った値に適用するための関数である。
		この関数がすることは、いつか渡されるであろう初期状態sをstに適用して、値xと状態xstを取り出して、fにxを適用して、状態モナドfstを取り出して、xstを適用して、その結果を返す関数である。
	-}

	{-
		さて次はモナドを実際に使ってみる。
	-}
