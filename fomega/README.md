# fomega
fomegaはCalculus of constructionsにsubtypingを導入したものを基礎とする。

## 定義
Typeは組み込みの値であり、全ての値を型付けする。subtypingとして{ x | y | .. z }
というように型の和を作ることができる。この構文はCoqのInductiveのインライン版とい
うイメージである。ラムダ式とcaseはhaskellと同じようにある。

この場合、有効な値がsubtypeである値のみになってしまう。固い値を生成する構文とし
てnewtypeを導入する。
