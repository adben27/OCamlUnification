open List

type po  = U | V | W | X | Y | A | B | F of po | G of po * po | H of po * po * po (* Termes du premier ordre : d'abord U,V,W,X,Y des variables, puis les fonctions *)
exception Echec of string                                                                   
let cons t1 t2 l= (t1, t2)::l;;

let equal_first t1 t2 = (* Fonction utilie pour anti-unif, pour savoir si on a la meme fonction (donc mm nombre d'args) pour recursiver *) 
  match (t1, t2) with
  | (F _, F _) -> true
  | (G _, G _) -> true
  | (H _, H _) -> true
  | (_, _) -> t1 = t2;;

let arite t = match t with 
  | F(f) -> 1
  | G(g1,g2) -> 2
  | H(h1,h2,h3) -> 3
  | A | B -> 0
  | _ -> -1;; (* Arité des variables défini à -1, pour avoir un match exhaustif *)

(*Les systeme d'equation seront représentés par une liste de coupe (po,po)*)
(*e est l'expression (le couple (po,po)) à supprimer et acc est la liste qui est renvoyé sans l'expression e*)
let rec sup e = function
  |[] -> []
  |t::q -> if(t=e) then sup e q else t::(sup e q);;

(*Swap les elements d'un couple*)
let swap e = let (e1, e2)=e in (e2, e1);;

(*Applique la fonction f à chaque élement égaux à p*)
let rec map f p = function
  |[] -> []
  |t::q -> if(t=p) then (f t)::(map f p q) else t::(map f p q)