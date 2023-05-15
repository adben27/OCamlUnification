<div align="center">
  <center><h1>Rapport</h1></center>
</div>

### DOUZI Jibril 12100516               
### BENNOUAR Adel 12003494


# Présentation du sujet et de sa solution algorithmique
Le but du projet est d’implémenter en OCaml les algorithmes d’unification et 
d’anti-unification.

Pour implémenter l'algorithme d'unification on a choisit de représenter le 
systeme d'équation qui permet l'unification de deux termes du premier ordre par 
une liste de couple de terme du premier ordre, par exemple le système d'équation
`x=f(y) | y=a` (a est une constante) qui veut dire "remplacer tout les "x" par 
"f(y)" et tout les "y" par "a" sera représenté par la liste 
`[(Var "x", Func("f", [Var "y"])); (Var "y", Func("a", []))]`, le premier élément 
d'un couple est ce que l'on veut remplacer et le deuxième élément est ce par
quoi on veut remplacer le premier élément. et on applique ces substitutions à 
l'un des deux termes, et si on a une exception alors on applique ces substitutions à
l'autre terme.

Pour implémenter l'algorithme d'anti-unification on représente le système 
d'équation de la même manière que pour l'unification mais le premier élément du
couple est un couple qui est le même que celui de l'unification et le deuxième
élément est la variable qui va remplacer ces deux termes, par exemple on veut
faire l'anti-unification de `h(x,f(y),x)` avec `h(a,f(b),a)` on aura alors la
liste `[((Var "y", Func("b", [])), Var "Z1"); ((Var "x", Func("a", [])), Var "Z0)]`
et on applique renvoie le terme "Var Zi" si le pattern des deux termes du premier
ordre qu'on veut anti-unifier est dans la liste des substitutions.


# Choix d’implémentation et des problèmes rencontrés

On a defini le type `po` qui a deux constructeur, le constructeur `Var` qui 
prend une chaine de caractère, ce constructeur représente les variables, et le
constructeur `Func` qui prend un couple. Le premier élément de ce couple est une 
chaine de caractère qui représente son nom, et le deuxième élément est une liste
de terme du premier ordre qui représente ses arguments.

Un des problèmes majeurs qu'on a rencontrés est que on devait écrire souvent 
la même fonction mais avec des arguments different par exemple pour avoir la 
liste des substitutions à faire pour l'unification, on avait une fonction qui 
prend en argument deux termes du premier ordre et une autre qui prend deux 
listes de terme du premier ordre, la fonction qui prend deux listes nous est 
utile car elle permet de faire la liste des substitutions sur les arguments des
fonctions (celle du premier ordre), la fonction qui prend des termes du premier 
ordre nous est utile car elle permet de faire la liste des substitutions avec 
une variable et un terme quelconque du premier ordre


# Listing du code

- <u>PRECISION</u>: On utilise le module List et dans ce module il y a un alias de `List`
           qui est `t` donc si vous voyez `List` ou `t` cela veut dire la même chose

--------------------------------------------------------------------------------
```
let string_of_po t = match t with
        | Var s -> s
        | Func(s, args) -> s;;

```

Type : `po -> string`
##### Renvoie le string en argument de `Var` ou `Func`
--------------------------------------------------------------------------------
```
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
```

Type : `print_po : po -> unit` et `print_list : po list -> unit`
##### Imprime un terme du premier ordre en utilisant notamment une fonction `print_list` qui print une liste
--------------------------------------------------------------------------------
```
let indice = ref 0
let varZ () =
  let i = !indice in
  indice := i+1;
  Var ("Z" ^ (string_of_int i))
```

Type : `unit -> po`
##### Cree une variable "Zi" utile pour "anti_unif"
--------------------------------------------------------------------------------
```
let arite t =
  begin 
    match t with 
    |Func(f, args) -> length args
    |Var x -> raise (Invalid_argument "Prend Func et pas Var comme argument")
  end
```

Type: `po -> int`
##### Donne l'arite de la fonction "t"
--------------------------------------------------------------------------------
```
let equal_first t1 t2 =
begin
  match (t1, t2) with
  |(Var x, Var y) -> x=y
  |(Func(f1, _), Func(f2, _)) -> (((arite t1)=(arite t2)) && (f1=f2))
  |(_, _) -> false (*Si on a Var et Func comme arguments*)
end
```

Type: `po -> po -> bool`
##### Dit si 2 termes sont égaux
--------------------------------------------------------------------------------
```
let rec renomage x t =
begin
  match t with
  |Var y ->if(x=y) then Var (x^"1") else Var y
  |Func(f, arg) ->Func(f, map (renomage x) arg)
end
```

Type: `string -> po -> po`
##### Renomme toutes les variables x qu'il y a dans t par "x1"
--------------------------------------------------------------------------------
```
let rec apparait x t =
begin
  match t with
  |Var y -> x=y
  |Func(_,args) -> exists (apparait x) args 
end
```

Type: `string -> po -> bool`
##### Dit si la variable "x" apparait dans le terme "t"
--------------------------------------------------------------------------------
```
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
```

Type: `po -> po -> po`
##### Renomme toutes les variables communes entre t1 t2
--------------------------------------------------------------------------------
```
let rec getAsso x l =
begin
  match l with
    |[] -> raise Not_found
    |t::q -> let (t1,t2)=t in
            if(t1=x) then t2 else getAsso x q
end
```

Type: `'a -> ('a * 'b) t -> 'b`
##### Cherche si dans la liste de couple "l" l'élément "x" apparait. Si oui, on renvoie son associé, sinon on lève une exception. Utile pour savoir quelle substitution faire
--------------------------------------------------------------------------------
```
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
```

Type: `po t -> po t -> (po * po) t`
##### Prend les arguments de 2 fonctions et envoie une liste de substitution pour  l'unification de ces 2 fonctions
--------------------------------------------------------------------------------
```
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
```

Type: `po -> po -> (po * po) t`
##### Comme 'list_subf_u' mais prend deux termes du premier ordre en arguments
--------------------------------------------------------------------------------
```
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
```

Type: `(po *po) t -> po -> po`
##### Substitue tous les termes t1 de subl par t2 dans la liste l (l est une liste de couple (t1,t2))
--------------------------------------------------------------------------------
```
let unif po1 po2=
let t2 = getVcommun po1 po2 in
let t1 = getVcommun po2 po1 in
let list = list_subs_u t1 t2 in
begin
  try (sub_lu list t1) with
  |Not_found -> sub_lu list t2
  |Echec x -> sub_lu list t2
end
```

Type: `po -> po -> po`
##### Unifie les termes po1 et po2
--------------------------------------------------------------------------------
```
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
```

Type: `po t -> po t -> ((po * po) * po) t`
##### Renvoie la liste des substitutions des arguments de 2 fonctions pour anti-unif
--------------------------------------------------------------------------------
```
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
```

Type: `po -> po -> ((po * po) * po) t`
##### Renvoie la liste des substitutions pour anti-unif
--------------------------------------------------------------------------------
```
let rec anti_unif t1 t2 =
begin
  match (t1,t2) with
    |(Var x, _) -> (getAsso (t1,t2) (list_sub_au t1 t2))
    |(_, Var x) -> (getAsso (t1,t2) (list_sub_au t1 t2))
    |(Func(f, arg1), Func(g, arg2)) when (equal_first t1 t2) -> Func(f, map2 anti_unif arg1 arg2)
    |(Func(f, arg1), Func(g, arg2)) when (not(equal_first t1 t2)) && f=g -> raise (Echec "Certaines fonctions n'ont pas la même arite")
    |_ -> varZ()
end
```

Type: `po -> po -> po`
##### Fait l'anti-unification des termes t1 et t2
--------------------------------------------------------------------------------

# Jeux d’essais

Pour faciliter la lisibilité des termes du premier ordre, on va noter les variables 
par des majuscules et les fonctions par des minuscules au lieu d'utiliser `Var` et 
`Func`.

## Jeux d’essais avec l'unification

```
unif X f(Y,a) -> f(Y,a)
unif f(Y,Y) g(X) -> TOP
unif f(Y,Y) f(X) -> TOP
unif f(f(X)) f(X) -> f(f(X))
unif f(Y,Y) f(a,b) -> f(a,a)
unif f(X,Y) f(a,g(Z)) -> f(a,g(Z))
unif f(X,g(Y,U)) f(a,g(Z)) -> TOP
unif f(X,g(Y,U)) f(a,g(Z,P)) -> f(a,g(Z,P))
unif h(X,f(Y),a) h(X,f(a),Y) -> h(X1,f(a),a)
```

## Jeux d’essais avec l'anti-unification

```
anti_unif X f(Y,a) -> Z0
anti_unif f(Y,Y) g(X) -> Z0
anti_unif f(Y,Y) f(X) -> Echec
anti_unif f(Y,Y) f(a,b) -> f(Z1,Z2) (bug pas corrige)
anti_unif f(X,Y) f(a,g(Z)) -> f(Z1,Z2)
anti_unif f(X,g(Y,U)) f(a,g(Z)) -> Echec
anti_unif f(X,g(Y,U)) f(a,g(Z,P)) -> f(Z2,g(Z3,Z4))
unif h(X,f(Y),X) h(a,f(b),a) -> h(Z0, f(Z1), Z2) bug doit renvoyer h(Z0, f(Z1), Z0)
```
