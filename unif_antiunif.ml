open List

(*string est le nom de la variable/fonction*)
type po = Var of string | Func of string * po list
exception Echec of string

let indice = ref 0

let varZ () =
  let i = !indice in
  indice := i+1;
  Var ("Z" ^ (string_of_int i))

(*Les constantes s'écriront Func("a", [])*)
let arite t =
  begin 
    match t with 
    |Func(f, args) -> length args
    |Var x -> raise (Invalid_argument "Prend Func et pas Var comme argument")
  end

let equal_first t1 t2 =
begin
  match (t1, t2) with
  |(Var x, Var y) -> x=y
  |(Func(f1, _), Func(f2, _)) -> (arite t1)=(arite t2) && f1=f2
  |(_, _) -> false (*Si on a Var et Func comme arguments*)
end

let rec renomage x t =
begin
  match t with
  |Var y ->if(x=y) then Var (x^"1") else Var y
  |Func(f, arg) ->Func(f, map (renomage x) arg)
end

let rec apparait x t =
begin
  match t with
  |Var y -> x=y
  |Func(_,args) -> exists (apparait x) args (*exists est comme map mais renvoie un bool qui est egale à  
                                              f arg1 || f arg2 || ... || f argn (f est une fonction qui renvoie un bool)*) 
end

let rec getVcommun t1 t2 =
begin
  match t1 with
  |Var x -> if(apparait x t2) then Some x else None
  |Func(f, arg) -> begin
                   match arg with
                   |[] -> None
                   |t::q -> let po=t in
                            getVcommun po t2;
                   end
end

(*Cherche si dans la liste de couple l l'élément x apparait si oui on renvoie son associer sinon on leve une exception*)
(*Exemple: getAsso 1 [(2,4);(3,0);(1,2)] renvoie 2*)
(*Utile pour savoir quelle substitution faire*)
let rec getAsso (x : po) (l : (po * po) t) =
begin
  match l with
    |[] -> raise Not_found
    |t::q -> let (t1,t2)=t in
            if(t1=x) then t2 else getAsso x q
end

(*Prend les arguments de 2 fonctions et envoie une liste de substitution pour l'unification de ces 2 fonctions*)
let rec sub_listf l1 l2=
begin
  match (l1, l2) with
  |([], []) -> []
  |(t::q, []) | ([], t::q)-> []
  |(t1::q1, t2::q2) -> begin
                        match (t1, t2) with
                        |(Var x, Var y) when x=y -> (sub_listf q1 q2)
                        |(Var x, _) when (apparait x t2) -> raise (Echec "non unifiable")
                        |(Var x, _) -> (Var x, t2)::(sub_listf q1 q2)
                        |(_, Var y) when (apparait y t1) -> raise (Echec "non unifiable")
                        |(_, Var y) -> (Var y, t1)::(sub_listf q1 q2)
                        |(Func(f, arg1), Func(g, arg2)) when (equal_first t1 t2) -> (sub_listf arg1 arg2)@(sub_listf q1 q2)
                        |_ -> raise (Echec "non unifiable")
                       end
end

(*Exemple: list_subs (Var "x") (Func("f", [Var "x"; Var "y"; Var "z"]) va renvoyez 
  la liste de substitution [(Var "x", (Func("f", [Var "x"; Var "y"; Var "z"])))]
  qui veut dire qu'on remplace x par f(x,y,z)*)
let rec list_subs t1 t2 =
begin
  match (t1, t2) with
  |(Var x, Var y) when x=y -> []
  |(Var x, _) when (apparait x t2) -> raise (Echec "non unifiable")
  |(Var x, _) -> [(Var x, t2)]
  |(_, Var y) when (apparait y t1) -> raise (Echec "non unifiable")
  |(_, Var y) -> [(Var y, t1)]
  |(Func(f, arg1), Func(g, arg2)) when (equal_first t1 t2) -> sub_listf arg1 arg2
  |_ -> raise (Echec "non unifiable") (*Pour avoir un match exhaustive*)
end

(*Substitue tout les termes t1 de subl par t2 dans la liste l (l est une liste de couple (t1,t2))*)
(*Exemple: sub_l (Some [(Var "y", Var "A");(Var "x", Var "B")]) [Func("f", [Var "y"; Var "x"]); Func("f", [Var "x"; Var "y"])];;*)
(*va renvoyez (::) (Func ("f", [Var "A"; Var "B"]), [Func ("f", [Var "B"; Var "A"])])*)

let rec sub_l (subl : (po*po) t  ) (l : po )=
begin
  match subl with
  |[] -> l
  |t::q -> begin
            match l with
              |Var x -> (getAsso (Var x) subl)
              |Func(f, arg) -> (Func(f, map (sub_l subl) arg))
          end
end

(*Swap les elements de la liste (contient les substitutions que l'on doit faire) si besoin*)
let rec swap = 
  begin 
  function 
  |[] -> []
  |t::q -> match t with
            |(Func(f, arg), Var x) -> (Var x, Func(f, arg))::(swap q)
            |(_,_) -> t::(swap q)
  end

let unif po1 po2= 
begin
  try (sub_l (list_subs po1 po2)) po1 with
  |Not_found -> sub_l (list_subs po2 po1) po2
  |Echec x-> Var "TOP"
end

(*-----------------------------Fonction qui ne servent a rien pour l'instant---------------------------*)
(*Comme sub_l mais pour un seul terme*)
let rec sub_t subl t=
begin
  match t with
  |Var x -> getAsso (Var x) subl
  |Func(f, arg) -> Func(f, map (sub_l subl) arg)
end

(*Supprime les couples (Var x, Var x) dans la liste (contient les substitutions que l'on doit faire)*)
let rec sup = 
begin 
  function
  |[] -> []
  |t::q -> 
          begin
            match t with
            |(Var x, Var y) -> if(x=y) then sup q else t::(sup q)
            |(_,_) -> t::(sup q)
          end
end

