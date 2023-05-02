type po  = U | V | W | X | Y | A | B | F of po | G of po * po | H of po * po * po (* Termes du premier ordre : d'abord U,V,W,X,Y des variables, puis les fonctions *)
                                                                                      
let n=0;;
let list=[];; 

let cons t1 t2 = (t1, t2, n+1)::list;;

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
              
let rec longueur l = 
  if (l =[]) then 0
  else 1+(longueur (List.tl l)) ;; (*Utile pour anti_unif pour savoir si on doit cree un nv element de list ou non. Exemple: si list a une taille de 4 et qu'on veut l'element 5 on cree un nv element sinon on recupere l'element 4*) 
  
let test1 = cons (F(X)) Y; Printf.printf "%d\n" n;;
  