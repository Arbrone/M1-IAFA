Require Import Lia.
Require Import Strings.String Nat Lists.List.
Import ListNotations.

(*Local Open Scope string_scope.*)

Module EXP.

(* On définit le type inductif des expressions. Une expression est une
   variable, une constante, une somme, une différence ou une
   expression conditionnelle. *)
Inductive exp : Type :=
  | Var (n : string)
  | Const (c : nat)
  | Add (e1 e2 : exp)
  | Sub (e1 e2 : exp)
  | IfThen (c e1 e2 : exp).

(* Représentation par des 'exp' des variables a et b *)
Definition A := Var "a".
Definition B := Var "b".

(* représentation de l'expression
   (b - (a + 1)) ? a+b : a+3 *)
Definition exp_example :=
  (IfThen (Sub B (Add A (Const 1)))
          (Add A B)
          (Add A (Const 3))).

(* Donner la sémantique dénotationnelle des expressions. Elle est
   définie par une fonction 'eval' prenant en argument un
   environnement (une fonction associant à un nom de variable un
   entier naturel), une expression et renvoyant la valeur de
   l'expression (de type 'nat'). On considère, comme dans exp_example,
   qu'une expression est vraie si elle est non nulle. On utilisera un
   match pour tester la condition. *)
Fixpoint eval (env : string -> nat) (e : exp) : nat :=
  match e with
  | Var n         => env n
  | Const n       => n
  | Add a b       => eval env a + eval env b
  | Sub a b       => eval env a - eval env b
  | IfThen c a b  =>  match eval env c with
                      | 0 => eval env b
                      | _ => eval env a
                      end
  end.

(* Un environnement donnant aux variables a et b les valeurs 1 et 2. *)
Definition env_example v :=
  match v with
  | "a" %string => 1
  | "b" %string => 2
  | _ => 0
  end.

(* exemple d'évaluation d'une expression *)
Lemma eval_example : eval env_example exp_example = 4.
Proof.
  auto.
Qed.

(* Pour définir la sémantique opérationnelle à petits pas des
   expressions ont introduit une pile et le jeu d'instructions
   suivant : *)
Inductive asm : Type :=
  | stop
    (* arret de la machine *)
  | add (cnt : asm)
    (* on remplace les 2 entiers en sommet de pile par leur somme et
       on passe à l'instruction suivante 'cnt' *)
  | sub (cnt : asm)
    (* on remplace les 2 entiers en sommet de pile par la soustract du
       2eme élément de la pile au sommet de pile et on passe à
       l'instruction suivante 'cnt' *)
  | ifThen (ift iff : asm)
    (* si le sommet est 0, on passe à iff sinon à ift. Dans les 2 cas,
       le sommet est dépilé *)
  | Comp (e : exp) (cnt : asm).
    (* on empile la valeur de e et on continue avec cnt *)

(* un petit pas transforme l'état de machine représenté par le couple
   (stk,l) en (stk',l') où stk est une pile d'entier et l
   l'instruction courante.  L'évolution de l'état sera représenté avec
   la syntaxe introduite ci-dessous pouvant se lire: dans
   l'environnement env, un petit pas transforme l'état
   composé de la pile stk et des instructions l en stk' et l'.  *)

Reserved Notation "env |- << stk , l >> --> << stk' , l' >>" (at level 30).

(* Compléter la sémantique petits pas ci-dessous :
   - règle ssCompV: si l'instruction est un Comp (Var v) l, on empile
     la valeur de v et on passe à l
   - règle ssCompC: si l'instruction est un Comp (Const c) l, on
     empile c et on passe à l
   - règle ssCompA: si l'instruction est un Comp (Add e1 e2) l, on
     demande le calcul de e2 puis de e1, on fait l'addition et on
     enchaîne sur l. Pour cela, on construit la nouvelle séquence
     d'instructions (Comp e2 (Comp e1 (add l))).
   - règle ssCompS: si l'instruction est un Comp (Sub e1 e2) l, on
     demande le calcul de e2 puis de e1, on fait la soustraction et on
     enchaîne sur l
   - règle ssCompI: si l'instruction est un Comp (IfThen c e1 e2) l,
     on demande le calcul de c et on continue avec 'ifThen'
   - règle ssExecA: si l'instruction est un add l et que la pile a au
     moins 2 éléments, on les remplace par leur somme et on enchaîne
     sur l
   - règle ssExecS: si l'instruction est un sub l et que la pile a au
     moins 2 éléments, on les remplace par leur différence et on
     enchaîne sur l
   - règle ssExecI: si l'instruction est un ifThen l1 l2 et que le
     sommet de pile est >0, on le dépile et on echaïne sur l1, sinon
     sur l2. On pourra exprimer cette règle via 2 consructeurs ou via
     un constructeur comportant un test (via match ... end) sur la
     valeur en sommet de pile. *)
Inductive smallStep (env : string -> nat) : list nat -> asm -> list nat -> asm -> Prop :=
  | ssCompV : forall stk v l, env |- << stk, (Comp (Var v) l) >> --> << (env v :: stk), l >>
  | ssCompC : forall stk c l, env |- << stk, (Comp (Const c) l) >> --> << (c :: stk), l >>
  | ssCompA : forall stk a b l, env |- << stk, (Comp (Add a b) l) >> --> << stk, (Comp a (Comp b (add l))) >>
  | ssCompS : forall stk a b l, env |- << stk, (Comp (Sub a b) l) >> --> << stk, (Comp a (Comp b (sub l))) >>
  | ssCompI : forall stk c e1 e2 l, env |- << stk, (Comp (IfThen c e1 e2) l) >> --> << stk, (Comp c (ifThen (Comp e1 l) (Comp e2 l))) >>
  | ssExecA : forall stk a b l, env |- << a::b::stk, add l >> --> << ((a + b)::stk), l >>
  | ssExecS : forall stk a b l, env |- << a::b::stk, sub l >> --> << ((a - b)::stk), l >>
  | ssExecI : forall stk c e1 e2, env |- << c::stk, ifThen e1 e2 >> --> << stk, (if c=?0 then e2 else e1) >>
  
where "env |- << stk , l >> --> << stk' , l' >>" := (smallStep env stk l stk' l').

(* On essaie maintenant d'établir un lien entre les 2 sémantiques. On
   associe à une machine la valeur qu'elle est supposée calculer s'il
   n'y a pas d'erreur de pile.  *)

Inductive Result T :=
  | Value (v : T)   (* le résultat est un entier *)
  | StackError.  (* il y a eu une erreur de pile *)

(* Définir la fonction récursive terminale asm_sem prenant en argument
   une pile d'entiers, une instruction et renvoyant un Result nat.
   - si l est stop, la valeur est en sommet de pile si elle n'est pas
     vide. Autrement il y a erreur de pile.
   - si l est Comp e l', on continue sur l' après avoir empilé la
     valeur de e obtenue avec la fonction eval.
   - si l est add l', on dépile 2 valeurs et on continue sur l' après
     avoir empilé leur somme. Si la pile ne contient pas au moins 2
     valeurs, on renvoie StackError.
   - si l est sub l', on dépile (si possible) 2 valeurs et on continue
     sur l' après avoir empilé leur différence
   - si l est ifThen l1 l2, on dépile (si possible) une valeur et on
     continue sur l2 ou l1 selon que la valeur est nulle ou non *)

Fixpoint asm_sem (env : string -> nat) (stk : list nat) (l : asm) : Result nat :=
  match l with
  | stop =>
      match stk with
      | [] => StackError _
      | v :: _ => Value _ v
      end
  | Comp e l' => eval e l'
  
  | add l' => let v1 := match (asm_sem env stk stop) with |Value _ v => v 
                                                          |_ => 0 end in
                
                let v2 := match (asm_sem env stk stop) with |Value _ v => v 
                                                            | _ => 0 end in
                  let r := v1 + v2 in
                    (asm_sem env stk (Comp (Add v1 v2) l'))
                
  | sub l' => let v1 := (asm_sem env stk stop) in
                let v2 := (asm_sem env stk stop) in 
                  let r := v1 - v2 in
                    (asm_sem env stk (Comp r l'))
  
  | ifThen l1 l2 => let c := (asm_sem env stk stop) in
                      match c with
                      | 0 => (asm_sem env stk (Comp _ l2))
                      | _ => (asm_sem env stk (Comp _ l1))
                      end                                                    _
  | _ => StackError _
  end.
  

(* depuis son état initial (demandant d'évaluer e dans une pile vide),
   le résultat supposé de la machine est la valeur attendue *)

Lemma msem_init : forall env e, asm_sem env [] (Comp e stop) = Value _ (eval env e).
Proof.
  (* TODO *)
Admitted.

(* depuis son état terminal (matérialisé par l'instruction stop et une
   pile à un élément), le résultat supposé, situé en sommet de pile,
   est la valeur attendue *)

Lemma msem_end : forall env v, asm_sem env [v] stop = Value _ v.
Proof.
  reflexivity.
Qed.

(* Démontrer la propriété suivante indiquant qu'un petit pas préserve
   la valeur supposée être calculée par la machine. 'inversion H; ...'
   génère tous les cas et effectue des traitements communs à ces
   cas. D'autres traitements pourront être factorisés si besoin. Par
   exemple "try lia" permettrait de prouver tous les sous-buts
   arithmétiques sans échouer s'il existe des sous-buts non
   arithmétiques. On obtient de 2 à 7-8 cas selon les traitements
   factorisés.  *)

Lemma asm_sem_preserved env stk l stk' l' :
  env |- <<stk, l>> --> <<stk', l'>> -> asm_sem env stk l = asm_sem env stk' l'.
Proof.
  intro H.
  inversion H; clear H; subst. (* on génère tous les cas *)
  (* - TODO ... *)
Admitted.

(* On en déduit donc qu'en faisant tourner la machine depuis l'état
   initial, si on arrive sur l'état terminal, on a bien calculé la
   valeur de l'expression. Il reste à montrer que la machine finit par
   s'arrêter et atteint bien l'état terminal. Afin de montrer la
   terminaison, on introduit un variant.  *)

(* Définir la fonction récursive size (e : exp) renvoyant le nombre de
   constructeurs de e sachant que les constructeurs Add et Sub
   comptent double. *)

Fixpoint size (e : exp) := 1.  (* TODO *)

Example size_example : size exp_example = 16.
Proof.
  (* TODO *)
Admitted.

(* Montrer par induction que la taille est > 0 *)

Lemma size_nz : forall e, size e > 0.
Proof.
  induction e; simpl; auto; lia.
Qed.

(* On définit la fonction récursive steps (l:asm) renvoyant un
   majorant du nombre de pas nécessaires pour traiter l'instruction
   l. *)

Fixpoint steps l :=
  match l with
  | stop => 0
  | Comp e l' => (size e + steps l')
  | add l' => S (steps l')
  | sub l' => S (steps l')
  | ifThen l1 l2 => S (max (steps l1) (steps l2))
  end.

(* montrer que le nombre de pas décroit strictement après un petit pas
   Une bonne factorisation des traitements réduit à 2 le nombre de cas
   à traiter de manière spécifique.  On utilisera 'specialize (size_nz
   e)' pour ajouter cette proporiété aux hypothèses lorsque ce sera
   nécessaire.  *)
Lemma variant_decreases env stk l stk' l' :
  env |- <<stk, l>> --> <<stk', l'>> -> steps l' < steps l.
Proof.
  intro H; inversion H; clear H.
  (* TODO *)
Admitted.

(* checkstk l n indique si une pile contenant n éléments est
   suffisante pour exécuter les instructions de l. Cette relation
   inductive est définie par les règles suivantes :

    n>0    checkStk l n                                   n>0
  ---------------------- (csAdd)    idem pour csSub  --------------- (csStop)
  checkstk (add l) (S n)                             checkstk stop n

      checkstk l (S n)
  ------------------------ (csComp)
    checkStk (Comp e l) n

    checkstk l1 n      checkstk l2 n
  ----------------------------------- (csIfThen)
      checkstk (ifThen l1 l2) (S n)

*)

Inductive checkStk : asm -> nat -> Prop :=
  | csStop : forall n, n > 0 -> checkStk stop n
(* | ... TODO *).

(* une pile vide est suffisante pour évaluer une expression *)
Lemma InitStk_OK : forall e, checkStk (Comp e stop) 0.
Proof.
Admitted.

(* on montre que si une pile est suffisante avant un petit pas, elle
   reste suffisante après. Autrement dit, une pile initialement vide
   est suffisante pour lancer l'évaluation d'une expression.  *)
Lemma StkOKPreserved env stk l stk' l' :
  checkStk l (length stk) ->
  env |- <<stk, l>> --> <<stk', l'>> ->
  checkStk l' (length stk').
Proof.
  intros H1 H2.
  inversion H2; clear H2; subst; simpl in *; auto;
    try (inversion H1; clear H1; subst; now auto).
  (* TODO
  - inversion H1; clear H1; subst; auto.
    admit.
  *)
Admitted.

(* la compilation d'une expression ne bloque pas la machine: il est
   toujours possible de faire au moins un petit pas. La preuve se fait
   par cas sur l'expression e via 'destruct e'.  le nom contient
   "noDL" pour "no deadlock" *)

Lemma Comp_noDL env stk l e :
  exists (stk' : list nat) (l' : asm),
    env |- << stk, Comp e l >> --> << stk', l' >>.
Proof.
  (* TODO *)
Admitted.

(* Si la pile est suffisante et que l'instruction n'est pas stop, on
   peut faire un petit pas. Autrement dit, l'exécution finit toujours
   par s'arréter sur stop. *)

Lemma stkOK_noDL env stk l :
  checkStk l (length stk) -> l <> stop ->
  exists stk' l', env |- <<stk, l>> --> <<stk', l'>>.
Proof.
  intros H1 H2.
  inversion H1; clear H1; subst; try tauto; clear H2.
  (* TODO
  - destruct stk as [ | v1 stk]; simpl in *; try lia.
    (* la pile contient au moins 1 element -> on le met dans v1 *)
    admit.
   *)
Admitted.

(* On a donc montré que :
   - l'exécution se termine
   - si la pile est suffisante avant un pas, elle reste suffisante
     après
   - si la pile est suffisante et que l'instruction n'est pas stop, un
     pas est possible
   - une pile vide suffit pour lancer l'évaluation d'une expression.
     Donc l'exécution se termine sur stop.
   - la valeur calculée étant préservée, la valeur de l'expression à
     l'arrêt de la machine est en sommet de pile. *)
End EXP.

Module EXCEXP.

(****************************)
(*                          *)
(* LA SUITE EST FACULTATIVE *)
(*                          *)
(****************************)

(* On recommence en ajoutant des exceptions: une exception est levée
   si on tente de calculer e1 - e2 avec e1 < e2.
   Try permet de récupérer l'exception *)

Inductive exp : Type :=
  | Var (n : string)
  | Const (c : nat)
  | Add (e1 e2 : exp)
  | Sub (e1 e2 : exp)
  | IfThen (c e1 e2 : exp)
  | Try (e1 e2 : exp).

(* Définir la sémantique dénotationnelle sous forme d'une fonction
   eval renvoyant maintenant un option nat. Renvoyer None indique
   qu'une exception a été levée mais n'a pas été récupérée. On
   utilisera <=? pour comparer les entiers. *)

Fixpoint eval (env : string -> nat) (e : exp) : option nat := None.  (* TODO *)

(* La machine utilisée pour définir la sémantique petits pas comporte
   toujours une pile d'entiers. Le jeu d'instructions est maintenant
   le suivant : *)

Inductive asm : Type :=
  | stop
    (* arret normal *)
  | fail
    (* levee d'exception *)
  | add (cnt : asm)
    (* ajout des 2 éléments en sommet de pile et continuation sur asm *)
  | sub (cnt : asm) (exc : list nat * asm)
    (* soustraction et continuation en cas de résultat >= 0, pile et
       instruction de récupération si le résultat est <0 *)
  | ifThen (ift iff : asm)
    (* continuation en fonction de la valeur du sommet de pile *)
  | Comp (e : exp) (cnt : asm) (exc : list nat * asm).
    (* évaluation de e, continuation en cas de succès ou d'exception *)

Reserved Notation "env |- << stk , l >> --> << stk' , l' >>" (at level 30).

(* Sémantique à petits pas. Les règles sont similaires excepté pour
   - Try e1 e2: en cas d'erreur de calcul de e1, on prévoie le calcul
     de e2 dans la pile courante.
   - sub l x: si le résultat est négatif, on récupère dans x la pile
     et l'instruction. *)

Inductive smallStep env : list nat -> asm -> list nat -> asm -> Prop :=
  | ssCompV : forall stk v l x, env |- << stk, (Comp (Var v) l x) >> --> << (env v :: stk), l >>
(* | TODO *)
where "env |- << stk , l >> --> << stk' , l' >>" := (smallStep env stk l stk' l').

(* Un résultat est maintenant une valeur, une erreur de manipulation
   de pile, ou une exception *)
Inductive Result T :=
  |  Value (v:T)
  | StackError
  | Exception.

(* Définir la fonction récursive terminale asm_sem. Le schéma est
   similaire au précédent mais
   - si l'évaluation de Comp e l x renvoie None, on enchaîne sur la
     pile et l'instruction contenus dans x
   - pour sub l x, si le résutat est négatif on continue avec la pile
     et l'instruction contenus dans x
   - l'évaluation de fail renvoie Exception *)

Fixpoint asm_sem (env : string -> nat) (stk : list nat) (l : asm) {struct l} : Result nat :=
  match l with
  | stop =>
      match stk with
      | [] => StackError _
      | v :: _ => Value _ v
      end
  | _ => Exception _  (* TODO *)
  end.

(* Montrer le lien entre l'évaluation d'une machine initiale et la
   valeur de l'expression *)

Lemma asm_sem_init : forall env e,
  match asm_sem env [] (Comp e stop ([], fail)) with
  | Value _ v => eval env e = Some v
  | StackError _ => False
  | Exception _ =>  eval env e = None
  end.
Proof.
  (* TODO
  intros; simpl.
  destruct (eval env e); auto.
*)
Admitted.

(* Evaluation d'une machine dans l'état d'arrêt *)

Lemma asm_sem_end1 : forall env v, asm_sem env [v] stop = Value _ v.
Proof.
  reflexivity.
Qed.

(* Evaluation d'une machine dans l'état d'erreur *)

Lemma asm_sem_end2 : forall env stk, asm_sem env stk fail = Exception _.
Proof.
  reflexivity.
Qed.

(* Preuve de la préservation de la valeur d'une machine par un petit
   pas On utilisera les lemmes
   Compare_dec.leb_correct :
     forall m n : nat, m <= n -> (m <=? n) = true
   Compare_dec.leb_correct_conv :
     forall m n : nat, m < n -> (n <=? m) = false *)

Lemma asm_sem_preserved : forall env stk l stk' l',
  env |- <<stk, l>> --> <<stk', l'>> -> asm_sem env stk l = asm_sem env stk' l'.
Proof.
  (* TODO *)
Admitted.

End EXCEXP.

