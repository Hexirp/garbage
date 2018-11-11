/* 型クラス継承図のPrologでの表現。 */

/* 注意点 - not, \=などの否定述語を条件づけるために使う場合、最後に書くこと
   例 - alpha(X) :- 満たすべき性質, \+ 満たされないべき性質; 性質, \+ 性質. */

/* contact - Yから二項関係をたどってZに到達出来るかどうか調べる */
contact(X, Y, Z) :- call(X,Y,Z);
                    call(X,Y,W), contact(X,W,Z).

/* cl - 二つの値が等しい場合を除く */
cl(X, Y, Z) :- call(X, Y, Z), Y \= Z.

/* bind - 二つの値がXにより結びついているかどうか調べる */
bind(X,Y,Z) :- call(X,Y,Z); call(X,Z,Y).

/* brother - YとZが兄弟かどうか調べる */
brother(X, A, Y, Z) :- cl(X, A, Y), cl(X, A, Z), Y \= Z.

/* rehtorb - YとZが共通の子を持つかどうか調べる */
rehtorb(X, Y, Z, A) :- cl(X, Y, A), cl(X, Z, A), Y \= Z.

/* diamond - 菱形継承パターンが存在するか調べる */
diamond(X, A, B, C, D) :- brother(X, A, B, C), rehtorb(X, B, C, D), A \= D.

/* diamond3 - 三つに分かれた継承パターンが存在するか調べる */
diamond3(W, A, X, Y, Z, B) :- diamond(W,A,X,Y,B),diamond(W,A,Y,Z,B), X \= Z.

/* radderX - 梯子型の継承パターンが存在するか調べる。
   
   A -> B -> C 
   |    |    | 
   v    v    v 
   D -> E -> F 

   こういうの */
radder2(X, A, A_, B, B_, C, C_) :- diamond(X,A,A_,B,B_), diamond(X,B,B_,C,C_), A \= C, A \= C_, A_ \= C, A_ \= C_.

radder3(X,A,A_,B,B_,C,C_,D,D_) :- radder2(X,A,A_,B,B_,C,C_), radder2(X,B,B_,C,C_,D,D_), A \= D, A \= D_, A_ \= D, A_ \= D_.

/* 汎用述語のex化 */
:- op(600, xfx, =>).

X => Y :- cl(ex, X, Y).

ext(X, Y) :- contact(ex, X, Y).

bro(A, B, C) :- brother(ex, A, B, C).

reh(A, B, C) :- rehtorb(ex, A, B, C).

dia(A, B, C, D) :- diamond(ex, A, B, C, D).

rad2(A,S,D,F,G,H) :- radder2(ex,A,S,D,F,G,H).

rad3(A,S,D,F,G,H,J,K) :- radder3(ex,A,S,D,F,G,H,J,K).

/* ttgraph - 非平面グラフ(K3-3)の探索, stgraph - 非平面グラフ(K5)の探索 */

ttgraph(A,B,C,D,E,F) :- bind(ex,A,D), bind(ex,A,E), bind(ex,A,F), bind(ex,B,D), bind(ex,B,E), bind(ex,B,F), bind(ex,C,D), bind(ex,C,E), bind(ex,C,F), 
                        A \= B, A \= C, A \= D, A \= E, A \= F, B \= C, B \= D, B \= E, B \= F, C \= D, C \= E, C \= F, D \= E, D \= F, E \= F.

stgraph(A,B,C,D,E) :- bind(ex,A,B), bind(ex,B,C), bind(ex,C,D), bind(ex,D,E), bind(ex,E,A), bind(ex,A,C), bind(ex,C,E), bind(ex,E,B), bind(ex,B,D), bind(ex,D,A),
                      A \= B, A \= C, A \= D, A \= E, B \= C, B \= D, B \= E, C \= D, C \= E, D \= E.

isnot_plane :- ttgraph(_,_,_,_,_,_); stgraph(_,_,_,_,_).
 
/* ex(親, 子) - extend */
ex(type, unitype).
/*
ex(type, nullitype).
ex(type, dutype).
ex(type, tritype).
*/

ex(unitype, wrapper).
ex(unitype, unwrapper).
ex(unitype, conquer).
ex(unitype, invariant).
ex(unitype, unquer).

ex(invariant, functor).
ex(invariant, contravariant).

ex(wrapper, identity).
ex(wrapper, pointed).

ex(unwrapper, identity).
ex(unwrapper, copointed).

ex(conquer, contrapointed).
ex(conquer, contity).

ex(unquer, untrapointed).
ex(unquer, contity).

ex(functor, apply).
ex(functor, coapply).
ex(functor, pointed).
ex(functor, copointed).
ex(functor, nonvariant).

ex(contravariant, nonvariant).
ex(contravariant, divide).
ex(contravariant, contrapointed).
ex(contravariant, codivide).
ex(contravariant, untrapointed).

ex(pointed, applicative).

ex(copointed, extract).

ex(contrapointed, divisible).

ex(untrapointed, codivisible).

ex(apply, bind).
ex(apply, applicative).

ex(coapply, extend).
ex(coapply, extract).

ex(divide, divisible).

ex(codivide, codivisible).

ex(applicative, monad).

ex(extract, comonad).

ex(bind, monad).

ex(extend, comonad).

/*
ex(dutype, profunctor).

ex(dutype, bifunctor).
ex(dutype, swap).
ex(dutype, semigroupoid).

ex(bifunctor, biapply).

ex(swap, and).
ex(swap, or).

ex(semigroupoid, category).
ex(semigroupoid, lifted).
ex(semigroupoid, parallel).

ex(category, arrow).

ex(lifted, arrow).

ex(parallel, arrow).
*/

/* co(一, 二) - 圏論的な双対 */
co(functor, functor).
co(wrapper, unwrapper).
co(apply, coapply).
co(pointed, copointed).
co(bind, extend).
co(applicative, extract).
co(monad, comonad).

co(contravariant, contravariant).
co(conquer, unquer).
co(divide, codivide).
co(contrapointed, untrapointed).
co(divisible, codivisible).

/* ct(一, 二) - 圏論的な反変 */
ct(functor, contravariant).

ct(wrapper, conquer).
ct(apply, divide).
ct(pointed, contrapointed).
ct(applicative, divisible).

ct(unwrapper, unquer).
ct(coapply, codivide).
ct(copointed, untrapointed).
ct(extract, codivisible).

/* dm - AndとOrの関係 */
dm(or,and).

/* pair - 対になっていること */
pair(A,B) :- bind(co,A,B); bind(ct,A,B); bind(dm,A,B).

/* bi - 二重化 */
bi(functor, bifunctor).
bi(apply, biapply).