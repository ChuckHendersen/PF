type expr =
Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr;;

let rec subexpr e1 e2 =
  e1=e2 or
  match e1 with
    Sum(x,y) | Diff (x,y) | Mult(x,y) | Div(x,y) -> subexpr x e2 || subexpr y e2
  |_->false;;

let rec subst_in_expr e x e1 = 
  match e with
  Var(y)-> if(x=y) then e1 (*Il confronto avviene sulle stringhe in realtà*)
            else e
  |Int(_)-> e
  |Sum(y,z)-> Sum(subst_in_expr y x e1,subst_in_expr z x e1)
  |Diff(y,z)-> Diff(subst_in_expr y x e1,subst_in_expr z x e1)
  |Mult(y,z)-> Mult(subst_in_expr y x e1,subst_in_expr z x e1)
  |Div(y,z)-> Div(subst_in_expr y x e1,subst_in_expr z x e1);;

  (*Tacci sua quanto è tosto ragionare in questo modo*)

type ’a tree = Empty | Tr of ’a * ’a tree * ’a tree;;

let rec reflect = function
Empty->Empty
|Tr(x,left,right)-> Tr(x,reflect right, reflect left);;


let fulltree h = 
  let rec aux h k= 
    if h=0 then Empty
    else Tr(k,aux(h-1)(k*2),aux(h-1)(k*2+1))
  in aux h 1;;

let rec height = function
  Empty->0
  |Tr(_,left,right)-> 1 + max(height left)(height right);;

let rec balanced = function
 Empty->true
 |Tr(_,left,right)-> balanced(left)&&balanced(right)&&(abs(height(left)-height(right)))<=1;;
