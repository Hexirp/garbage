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

Fixpoint hype' (f : nat -> nat -> nat -> nat) (m : nat) (n : nat) (o : nat) : nat :=
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
 | S n' => f m (hype' f m n' o) o
 end.

Fixpoint hype (m : nat) (n : nat) (o : nat) : nat :=
 match o with
 | Z => S n
 | S o' => (fix hype' (n_ : nat) : nat :=
  match n_ with
  | Z =>
   match o' with
   | Z => m
   | S o'' =>
    match o'' with
    | Z => Z
    | S o''' => S Z
    end
   end
  | S n_' => hype m (hype' n_') o'
  end) n
 end.

Fixpoint hyper (m : nat) (n : nat) (o : nat) :=
 match o with
 | Z => S n
 | S o' => hyper m (hype' hype m n o') o'
 end.