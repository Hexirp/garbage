Inductive nat : Type :=
 | Z : nat
 | S : nat -> nat.

Definition succ (n : nat) : nat := S n.

Fixpoint plus (m : nat) (n : nat) : nat :=
 match n with
 | Z => m
 | S n' => S (plus m n')
 end.

Fixpoint mult (m : nat) (n : nat) : nat :=
 match n with
 | Z => Z
 | S n' => plus m (mult m n')
 end.

Fixpoint powr (m : nat) (n : nat) : nat :=
 match n with
 | Z => S Z
 | S n' => mult m (powr m n')
 end.

Fixpoint tetr (m : nat) (n : nat) : nat :=
 match n with
 | Z => S Z
 | S n' => powr m (tetr m n')
 end.

Fixpoint pent (m : nat) (n : nat) : nat :=
 match n with
 | Z => S Z
 | S n' => tetr m (pent m n')
 end.

Fixpoint hype' (f : nat -> nat ) (m : nat) (n : nat) (o : nat) : nat :=
 match n with
 | Z =>
  match o with
  | Z => m
  | S o' =>
   match o' with
   | Z => Z
   | S o'' => S Z
   end
  end
 | S n' => f (hype' f m n' o)
 end.

Fixpoint hype (m : nat) (n : nat) (o : nat) : nat :=
 match o with
 | Z => S n
 | S o' => hype' (fun x => hype m x o') m n o'
 end.

Axiom tyre' : (nat -> nat) -> nat -> nat -> nat -> nat -> nat.

Fixpoint tyre (m : nat) (n : nat) (o : nat) (p : nat) : nat :=
 match p with
 | Z =>
  match o with
  | Z => S n
  | S o' => hype' (fun x => hype m x o') m n o'
  end
 | S p' => tyre' (fun x => tyre m n x p') m n o p'
 end.