Require Import Lia.

Module ENTIERS.

(* Definir la fonction récursive pow x n calculant x à la puissance n *)
  
Fixpoint pow (x:nat) (n:nat) :=
  match n with
    0 => 1
    | S i  =>  x * (pow x i)
  end.

(* On vérifie le type et le résultat de l'évaluation *)

Check pow.

Eval compute in (pow 2 3).

(* Démontrer que pow 2 3 = 8 *)
Example pow_2_3: pow 2 3 = 8.
Proof.
  auto.
Qed.

(* Démontrer par induction sur n la propriété suivante. On utilisera la tactique 'lia' *)

Lemma pow_1: forall n, pow 1 n = 1.
Proof.
  induction n.
  -
  auto.
  -
  simpl.
  rewrite IHn.
  auto.
Qed.

(* Démontrer par induction sur n la propriété suivante. On utilisera la tactique 'lia' *)

Lemma pow_add: forall x n m, pow x (n+m) = pow x n * pow x m.
Proof.
  induction n.
  -
  intro m.
  simpl.
  auto.
  - 
  intro m.
  simpl.
  rewrite IHn.
  lia.
Qed.

(* Démontrer par induction sur n la propriété suivante. On utilisera la tactique 'lia' *)

Lemma mul_pow: forall x y n, pow (x*y) n = pow x n * pow y n.
Proof.
  induction n.
  -
  auto.
  -
  simpl.
  rewrite IHn.
  lia.
Qed.

(* Démontrer par induction sur n la propriété suivante. On utilisera la tactique 'lia' ainsi que pow_1, mul_pow et pow_add *)

Lemma pow_mul: forall x n m, pow x (n*m) = pow (pow x n) m.
Proof.
  induction n.
  -
  intro m.
  simpl.
  rewrite pow_1.
  auto.
  -
  intro m.
  simpl.
  rewrite mul_pow.
  rewrite pow_add.
  rewrite IHn.
  lia.
Qed.

End ENTIERS.


Module LISTES.

Require Import List.
Include ListNotations.

(* Définir la fonction merge prenant en argument deux listes l1 et l2 d'éléments d'un type T quelconque et renvoyant la liste obtenue en prenant alernativement un élément de l1 puis un élément de l2. Si l'une des listes est vide, on prend les éléments de l'autre liste. *)

Fixpoint merge {T} (l1 l2: list T): list T := []. (* TODO *)

(* exemple *)
Example merge_example: merge [1;2;3] [4;5] = [1;4;2;5;3].
Proof.
  (* TODO *)
Admitted.


(* Démontrer par induction sur l1 la propriété suivante. On utilisera 
'destruct l2' pour examiner les cas l2 vide ou non vide, et 'lia' *)

Lemma merge_len: forall T (l1 l2: list T), length (merge l1 l2) = length l1 + length l2.
Proof.
  (* TODO *)
Admitted.

End LISTES.
