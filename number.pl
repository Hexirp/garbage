/* 自然数のPrologでの表現。 */

/* 注意点 - not, \=などの否定述語を条件づけるために使う場合、最後に書くこと
   例 - alpha(X) :- 満たすべき性質, \+ 満たされないべき性質; 性質, \+ 性質. */

/* zeroは0を表す。
   suc(X)は後者を表す。 */

:- op(100, fy, suc).

nat(zero).
nat(suc X) :- nat(X).

eq(zero, zero).
eq(suc X, suc Y) :- eq(X, Y).

plus(zero, Y, Y).
plus(suc X, Y, Z) :- suc A = Z, plus(X, Y, A).

mult(zero, _, zero).
mult(suc X, Y, Z) :- mult(X, Y, A), plus(A, Y, Z).

/*
mult(suc suc zero, zero, zero) :- mult(suc zero, zero, A), plus(A, zero, zero).
mult(suc zero, zero, A) :- mult(zero, zero, A_), plus(A_, zero, A).
mult(zero, zero, A_) :- matching: A_ = zero.
plus(zero, zero, A) :- matching: A = zero.
plus(zero, zero, zero) :- true.
> ;
dematching: A.
plus(zero, zero, suc zero) :- false.
plus(zero, zero, suc suc zero) :- false.
.
.
.
*/