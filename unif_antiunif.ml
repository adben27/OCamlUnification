(*E pour dire element vide utile pour get*)
type po  = E | U | V | W | X | Y | A | B | F of po | G of po * po | H of po * po * po (* Termes du premier ordre : d'abord U,V,W,X,Y des variables, puis les fonctions *)
                                                                                      
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

(*Permet de recuperer la x-ième valeur de la liste l, renvoie (E,E) si x>l.lenght*)
let rec get x l=
  match l with
  |[] -> (E,E)
  |t::q -> if(x=0) then t else get (x-1) q;;

