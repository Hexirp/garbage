Require Import Init.

Module base.

Inductive fin : nat -> Set :=
 | fo : forall n, fin n
 | fs : forall n, fin n -> fin (S n).

End base.

Module lambda.
Import base.

Inductive term (n : nat) : Set :=
 | var : fin n -> term n
 | lam : term (S n) -> term n
 | app : term n -> term n -> term n.

End lambda.

Module fomega.
Import base.

