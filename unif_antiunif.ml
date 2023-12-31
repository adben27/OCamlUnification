open List

(*string est le nom de la variable/fonction*)
type po = Var of string | Func of string * po list
exception Echec of string

let string_of_po t = match t with
        | Var s -> s
        | Func(s, args) -> s;;

let rec print_po p =
  match p with
  | Var s -> Printf.printf "%s" s
  | Func (f, args) ->
      Printf.printf "%s(" f;
      print_list args;
      Printf.printf ")"

and print_list = function
  | [] -> ()
  | [x] -> print_po x
  | x::xs -> print_po x; Printf.printf ", "; print_list xs;;
  
(*Cree une variable "Zi" utile pour "anti_unif"*)
let indice = ref 0
let varZ () =
  let i = !indice in
  indice := i+1;
  Var ("Z" ^ (string_of_int i))

(*Donne l'arite de la fonction "t"*)
(*Les constantes s'écriront Func("a", [])*)
let arite t =
  begin 
    match t with 
    |Func(f, args) -> length args
    |Var x -> raise (Invalid_argument "Prend Func et pas Var comme argument")
  end

(*Dit si 2 termes sont égaux*)
let equal_first t1 t2 =
begin
  match (t1, t2) with
  |(Var x, Var y) -> x=y
  |(Func(f1, _), Func(f2, _)) -> (((arite t1)=(arite t2)) && (f1=f2))
  |(_, _) -> false (*Si on a Var et Func comme arguments*)
end

(*Renome toute les variable x qu'il y a dans t par "x1"*)
let rec renomage x t =
begin
  match t with
  |Var y ->if(x=y) then Var (x^"1") else Var y
  |Func(f, arg) ->Func(f, map (renomage x) arg)
end

(*Dit si la variable "x" apparait dans le terme "t"*)
let rec apparait x t =
begin
  match t with
  |Var y -> x=y
  |Func(_,args) -> exists (apparait x) args (*exists est comme map mais renvoie un bool qui est egale à  
                                              f arg1 || f arg2 || ... || f argn (f est une fonction qui renvoie un bool)*) 
end

(*Renome toutes les variables communne entre t1 t2*)
let rec getVcommun t1 t2 =
begin
  match t1 with
  |Var x -> if(apparait x t2) then renomage x t2 else t2
  |Func(f, arg) -> begin
                   match arg with
                   |[] -> t2
                   |t::q -> if(apparait (string_of_po t) t2) then renomage (string_of_po t) t2
                   else getVcommun (Func(f, q)) t2
                   end
end

(*Cherche si dans la liste de couple "l" l'élément "x" apparait si oui on renvoie son associer sinon on leve une exception*)
(*Exemple: getAsso 1 [(2,4);(3,0);(1,2)] renvoie 2*)
(*Utile pour savoir quelle substitution faire*)
let rec getAsso x l =
begin
  match l with
    |[] -> raise Not_found
    |t::q -> let (t1,t2)=t in
            if(t1=x) then t2 else getAsso x q
end

(*Prend les arguments de 2 fonctions et envoie une liste de substitution pour l'unification de ces 2 fonctions*)
let rec list_subf_u l1 l2=
begin
  match (l1, l2) with
  |([], []) -> []
  |(t::q, []) | ([], t::q)-> []
  |(t1::q1, t2::q2) -> begin
                        match (t1, t2) with
                        |(Var x, Var y) when x=y -> (list_subf_u q1 q2)
                        |(Var x, _) when (apparait x t2) -> raise (Echec "TOP")
                        |(Var x, _) -> (Var x, t2)::(list_subf_u q1 q2)
                        |(_, Var y) when (apparait y t1) -> raise (Echec "TOP")
                        |(_, Var y) -> (Var y, t1)::(list_subf_u q1 q2)
                        |(Func(f, arg1), Func(g, arg2)) when (equal_first t1 t2) -> (list_subf_u arg1 arg2)@(list_subf_u q1 q2)
                        |_ -> raise (Echec "TOP")
                       end
end

(*Exemple: list_subs (Var "x") (Func("f", [Var "x"; Var "y"; Var "z"]) va renvoyez 
  la liste de substitution [(Var "x", (Func("f", [Var "x"; Var "y"; Var "z"])))]
  qui veut dire qu'on remplace x par f(x,y,z)*)
let rec list_subs_u t1 t2 =
begin
  match (t1, t2) with
  |(Var x, Var y) when x=y -> []
  |(Var x, _) when (apparait x t2) -> raise (Echec "TOP")
  |(Var x, _) -> [(Var x, t2)]
  |(_, Var y) when (apparait y t1) -> raise (Echec "TOP")
  |(_, Var y) -> [(Var y, t1)]
  |(Func(f, arg1), Func(g, arg2)) when (equal_first t1 t2) -> list_subf_u arg1 arg2
  |_ -> raise (Echec "TOP") (*Pour avoir un match exhaustive*)
end

(*Substitue tout les termes t1 de subl par t2 dans la liste l (l est une liste de couple (t1,t2))*)
(*Exemple: sub_l ([(Var "y", Var "A");(Var "x", Var "B")]) [Func("f", [Var "y"; Var "x"]); Func("f", [Var "x"; Var "y"])];;*)
(*va renvoyez (::) (Func ("f", [Var "A"; Var "B"]), [Func ("f", [Var "B"; Var "A"])])*)
let rec sub_lu subl t=
begin
  match subl with
  |[] -> t
  |t1::q -> begin
            match t with
              |Var x -> (getAsso (Var x) subl)
              |Func(f, arg) -> (Func(f, map (sub_lu subl) arg))
          end
end

(*Unifie les termes po1 et po2*)
(*Si il y a un try with c'est parceque dans certain cas (subl list po1) 
  renvoie Not_found (voir getAsso) alors qu'il ne devrai pas mais si on fait (subl list po2)
  au lieu de (subl list po1) on resout le problème du Not_found*)
let unif po1 po2=
let t2 = getVcommun po1 po2 in
let t1 = getVcommun po2 po1 in
let list = list_subs_u t1 t2 in
begin
  try (sub_lu list t1) with
  |Not_found -> sub_lu list t2
  |Echec x -> sub_lu list t2
end

(*Renvoie la liste des substitutions des arguments de 2 fonctions pour anti-unif*)
let rec list_subf_au l1 l2 =
begin
  match (l1, l2) with
  |([], []) -> []
  |(t::q, []) | ([], t::q) -> []
  |(t1::q1, t2::q2) -> begin
                        match (t1, t2) with
                        |(Var x, Var y) when x=y -> (list_subf_au q1 q2)
                        |(Var x, _) -> ((Var x, t2), varZ())::(list_subf_au q1 q2)
                        |(_, Var y) -> ((t1, Var y), varZ())::(list_subf_au q1 q2)
                        |(Func(f, arg1), Func(g, arg2)) when (equal_first t1 t2) -> (list_subf_au arg1 arg2)@(list_subf_au q1 q2)
                        |_ -> ((t1, t2), varZ())::(list_subf_au q1 q2) (*Jamais le cas car il est traite dans 'list_sub_au', c'est pour avoir un match exhaustive*)
                      end
end

(*Renvoie la liste des substitutions pour anti-unif*)
let list_sub_au t1 t2 =
begin
  match (t1,t2) with
  |(Var x, Var y) -> [((t1, t2), varZ())]
  |(Var x, _) -> [((t1, t2), varZ())]
  |(_, Var x) -> [((t1, t2), varZ())]
  |(Func(f, arg1), Func(g, arg2)) when ((equal_first t1 t2)) -> list_subf_au arg1 arg2
  |(Func(f, arg1), Func(g, arg2)) when (f<>g) && ((arite t1)=0) -> [((t1, t2), varZ())] (*f et g different*)
  |_ -> raise (Echec "Certaines fonctions n'ont pas la même arite")
end

(*Fait l'anti-unification des termes t1 et t2*)
let rec anti_unif t1 t2 =
begin
  match (t1,t2) with
    |(Var x, _) -> (getAsso (t1,t2) (list_sub_au t1 t2))
    |(_, Var x) -> (getAsso (t1,t2) (list_sub_au t1 t2))
    |(Func(f, arg1), Func(g, arg2)) when (equal_first t1 t2) -> Func(f, map2 anti_unif arg1 arg2)
    |(Func(f, arg1), Func(g, arg2)) when (not(equal_first t1 t2)) && f=g -> raise (Echec "Certaines fonctions n'ont pas la même arite")
    |_ -> varZ()
end

(*-----------------------------Fonction qui ne servent a rien pour l'instant---------------------------*)
(*Comme sub_lu mais pour un seul terme*)
let rec sub_tu subl t=
begin
  match t with
  |Var x -> getAsso (Var x) subl
  |Func(f, arg) -> Func(f, map (sub_tu subl) arg)
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

(*Swap les elements de la liste (contient les substitutions que l'on doit faire) si besoin*)
let rec swap = 
  begin 
  function 
  |[] -> []
  |t::q -> match t with
            |(Func(f, arg), Var x) -> (Var x, Func(f, arg))::(swap q)
            |(_,_) -> t::(swap q)
  end

(*revoie les substitution des arguments d'une fonction*)
let rec subf_lau subl l1 l2=
begin
  match (l1,l2) with
  |(t1::q1, t2::q2) -> begin
                        try (getAsso (t1,t2) subl) with
                        |Not_found -> subf_lau subl q1 q2 
                       end
  |_ -> varZ()
end
