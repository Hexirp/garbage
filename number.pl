/* ���R����Prolog�ł̕\���B */

/* ���ӓ_ - not, \=�Ȃǂ̔ے�q��������Â��邽�߂Ɏg���ꍇ�A�Ō�ɏ�������
   �� - alpha(X) :- �������ׂ�����, \+ ��������Ȃ��ׂ�����; ����, \+ ����. */

/* zero��0��\���B
   suc(X)�͌�҂�\���B */

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