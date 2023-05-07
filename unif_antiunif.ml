open List

(*string est le nom de la variable/fonction*)
type po = Var of string | Func of string * po list
exception Echec of string

let rec apparait x t =
  match t with
  |Var y -> x=y
  |Func(_,args) -> exists (apparait x) args (*exists est comme map mais renvoie un bool qui est egale à  
                                              f arg1 || f arg2 || ... || f argn (f est une fonction qui renvoie un bool)*) 

let equal_first t1 t2 = 
  match (t1, t2) with
  |(Var x, Var y) -> x=y
  |(Func(f1, arg1), Func(f2, arg2)) -> (length arg1)=(length arg2) && f1=f2
  |(_, _) -> false (*Si on a Var et Func comme arguments*)

(*Les constantes s'écriront Func("a", [])*)
let arite t = 
  match t with 
  |Func(f, args) -> length args
  |Var x -> raise (Invalid_argument "Prend Func et pas Var comme argument")

(*Cherche si dans la liste de couple l l'élément x apparait si oui on renvoie son associer sinon on leve une exception*)
(*Exemple: getAsso 1 [(2,4);(3,0);(1,2)] renvoie 2*)
(*Utile pour savoir quelle substitution faire*)
let rec getAsso x l = assoc x l

(*Exemple: unif (Var "x") (Func("f", [(Var "x");(Var "y");(Var "z")]) va renvoyez 
  la liste de substitution [(Var "x", (Func("f", [Var "x"; Var "y"; Var "z"])))]
  qui veut dire qu'on remplace x par f(x,y,z)*)
let rec list_subs t1 t2 =
  match (t1, t2) with
  |(Var x, Var y) when x=y -> Some []
  |(Var x, _) when (apparait x t2) -> raise (Echec "non unifiable")
  |(Var x, _) -> Some [(Var x, t2)]
  |(_, Var y) when (apparait y t1) -> raise (Echec "non unifiable")
  |(_, Var y) -> Some [(Var y, t1)]
  (*|(Func(f, arg1), Func(g, arg2) -> A faire*)
  |_ -> raise (Echec "non unifiable")

(*Swap les elements de la liste (contient les substitutions que l'on doit faire) si besoin*)
let rec swap = function 
  |[] -> []
  |t::q -> match t with
           |(Func(f, arg), Var x) -> (Var x, Func(f, arg))::(swap q)
           |(Var x, _) -> t::(swap q)
           |(_,_) -> t::(swap q)

(*Supprime les couples (Var x, Var x) dans la liste (contient les substitutions que l'on doit faire)*)
let rec sup = function
  |[] -> []
  |t::q -> match t with
           |(Var x, Func(f, arg)) -> t::(sup q)
           |(Var x, Var y) -> if(x=y) then sup q else t::(sup q)
           |(_,_) -> t::(sup q)