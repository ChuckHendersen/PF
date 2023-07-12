type expr =
Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr;;

let rec subexpr e1 e2 = 
  e1=e2 || match e1 with
  Int(_)-> false
  |Var(_)-> false
  |Sum(sube1,sube2)-> subexpr sube1 e2 || subexpr sube2 e2
  |Diff(sube1,sube2)-> subexpr sube1 e2 || subexpr sube2 e2
  |Mult(sube1,sube2)-> subexpr sube1 e2 || subexpr sube2 e2
  |Div(sube1,sube2)-> subexpr sube1 e2 || subexpr sube2 e2;;
(*Ottimizziamo la scrittura del codice raggruppando il codice duplicato*)

let rec subexpr e1 e2 = 
  e1=e2 || match e1 with
  |Sum(sube1,sube2)|Diff(sube1,sube2)|Mult(sube1,sube2)|Div(sube1,sube2) -> subexpr sube1 e2 || subexpr sube2 e2
  |_-> false;;

(*Estrapola nome variabile just for fun*)
let estrapolaNomeVar = function
  Var(x)-> x
  |_-> failwith "not a variable";;

let rec subst_in_expr e var e1 =
  match e with
  Int(_)->e
  |Var(x)-> if x=var then e1 else e
  |Sum(se1,se2)-> Sum(subst_in_expr se1 var e1,subst_in_expr se2 var e1)
  |Diff(se1,se2)-> Diff(subst_in_expr se1 var e1,subst_in_expr se2 var e1)
  |Mult(se1,se2)-> Mult(subst_in_expr se1 var e1,subst_in_expr se2 var e1)
  |Div(se1,se2)-> Div(subst_in_expr se1 var e1,subst_in_expr se2 var e1);;

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

let rec reflect = function
  Empty->Empty
  |Tr(x,left,right)-> Tr(x,reflect right,reflect left);;

let fulltree h = 
  let rec aux h k =
    match h with
    0-> Empty
    |n-> Tr(k,aux(n-1)(2*k),aux(n-1)((2*k+1)))
  in aux h 1;;

let rec height = function
  Empty->0
  |Tr(_,left,right)-> 1 + max (height left) (height right);;

let rec balanced = function
  Empty->true
  |Tr(_,left,right)-> abs(height(left)-height(right))<=1 && 
                      balanced (left) && balanced(right);;

let rec preorder = function
  Empty->[]
  |Tr(x,left,right)-> x::preorder(left) @ preorder(right);;

let rec postorder = function
  Empty->[]
  |Tr(x,left,right)-> postorder(left) @ postorder(right) @ [x];;

let rec inorder = function
  Empty->[]
  |Tr(x,left,right)-> inorder(left) @ [x] @ inorder(right);;

let rec take n aList = match n with
  0->[]
  |x-> (List.hd aList)::take(x-1)(List.tl aList);;

let rec drop n aList = match n with
  0-> aList
  |x-> drop(x-1)(List.tl aList);;


(*Che dito ar culo porca puttana*)  
let rec balpreorder aList = match aList with
 []->Empty
 |n::rest-> let k = (List.length (rest))/2 in
  Tr(n,balpreorder(take k rest),balpreorder(drop k rest));;

(*Peggio me sento per questa*)
let rec balinorder aList = match aList with
  []-> Empty
  |x::rest-> let k = List.length (x::rest)/2 in
    let n = List.hd (drop k (x::rest)) in
    Tr(n,balinorder(take k (x::rest)),balinorder(drop (k+1) (x::rest)));;

let rec foglie_in_lista lst = function
  Empty->true
  |Tr(f,Empty,Empty)-> List.mem f lst
  |Tr(n,left,right)-> foglie_in_lista lst left && foglie_in_lista lst right;;

let rec num_foglie = function
  Empty->0
  |Tr(f,Empty,Empty)->1
  |Tr(n,left,right)-> num_foglie left + num_foglie right;;

let rec segui_bool boolList t = match boolList with
 []-> t
 |b::rest-> 
  if t=Empty then Empty else let Tr(n,left,right)=t in
  if left=Empty && right=Empty then Empty
  else if b then segui_bool rest left else segui_bool rest right;;

let foglia_costo t= 
  let rec aux costo = function
    Empty-> failwith "Empty tree"
    |Tr(f,Empty,Empty)-> (f,f+costo)
    |Tr(n,left,Empty) -> aux (costo+n) left
    |Tr(n,Empty,right)-> aux (costo+n) right
    |Tr(n,left,right)-> let resL = aux (costo+n) left in
                        let resR = aux (costo+n) right in
                        if(snd(resL) > snd(resR)) then resL else resR
  in aux 0 t;;

let foglie_costi t =
  let rec aux t costo = match t with
  Empty->[]
  |Tr(f,Empty,Empty) -> [(f,f+costo)]
  |Tr(n,left,right) -> let nuovoCosto = n+costo in
    (aux left nuovoCosto @ aux right nuovoCosto)
in aux t 0;;

(*******************************************************************************)

type expr =
Jolly
| Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr;;

let pattern = Sum(Var("x"),Jolly);;

(*Qua credo serva la mutua ricorsione*)
let rec pattern_matching e pattern = 
  match (e,pattern) with
  (_,Jolly)->true
  |(Sum(e1,e2),Sum(m1,m2))|(Diff(e1,e2),Diff(m1,m2))
  |(Mult(e1,e2),Mult(m1,m2))|(Div(e1,e2),Div(m1,m2))-> pattern_matching e1 m1 && pattern_matching e2 m2
  |(e,m)-> e=m;;

(**********************************************************)

(*Vuole una corrispondenza di sotto albero 1:1, cioè se gia dalla radice iniziale i nodi sono diversi, l'albero sarà Empty<-@->Empty. Il sotto albero non va trovato ovunque ma a partire dalla radice di entrambi*)
let rec max_common_subtree t1 t2 = match (t1,t2) with
  (Tr(_,_,_),Empty)|(Empty,Tr(_,_,_))|(Empty,Empty)-> Empty
  |(Tr(a,aLeft,aRight),Tr(b,bLeft,bRight))-> 
    if a=b then Tr(a,max_common_subtree aLeft bLeft, max_common_subtree aRight bRight)
    else Tr("@",Empty,Empty);;

let stringTreeA = Tr("C",Tr("G",Empty,Empty),Tr("H",Tr("F",Empty,Empty),Tr("A",Empty,Empty)));;

let stringTreeB = Tr("C",Tr("G",Empty,Empty),Tr("H",Empty,Empty));;

let stringTreeC = Tr("C",Tr("G",Empty,Empty),Tr("H",Tr("P",Empty,Empty),Empty));;

let stringTreeD = Tr("A",Tr("G",Empty,Empty),Tr("H",Tr("F",Empty,Empty),Tr("A",Empty,Empty)));;

let rec stessa_struttura t1 t2 = match (t1,t2) with
(Empty,Empty)->true
|(_,Empty)|(Empty,_)->false
|(Tr(_,left1,right1),Tr(_,left2,right2))-> 
  stessa_struttura left1 left2 && stessa_struttura right1 right2;;

let rec is_funzione = function
  []->true                        (*Ammette duplicati*)
  |(x,y)::rest-> not(List.exists (fun (z,k)-> z=x&&k!=y ) rest) && is_funzione rest;;

let esiste_mapping t1 t2 = 
  let rec crea_lista t1 t2 = match (t1,t2) with
    (Empty,Empty)-> []
    |(_,Empty)|(Empty,_)-> failwith "non stessa struttura" (*Non dovrebbe mai capitare in questo caso*)
    |(Tr(x,leftx,rightx),Tr(y,lefty,righty))-> (x,y)::(crea_lista leftx lefty @ crea_lista rightx righty)
  in
  if stessa_struttura t1 t2 then is_funzione (crea_lista t1 t2)
  else false;;

let rec path p t = match t with
  Empty->[]
  |Tr(n,left,right)-> 
    if not(p n) then try n::path p left with _ -> n::path p right
    else failwith "predicato non soddisfatto";;

type 'a sostituzione = ('a * 'a tree) list

let subst = (4,Tr(5,Tr(25,Empty,Empty),Empty));;

let rec applica sostituzione = function
  Empty-> Empty
  |Tr(f,Empty,Empty)-> 
    if f = fst(sostituzione) then snd(sostituzione)
    else Tr(f,Empty,Empty)
  |Tr(n,left,right)-> Tr(n,applica sostituzione left, applica sostituzione right);;

let rec rm_from_lst e = function
  []->[]
  |x::rest-> 
    if x=e then rm_from_lst e rest
    else x::rm_from_lst e rest;;

(* Non funziona bene *)
let rec path_coprente t list = match t with
  Empty-> failwith "path_coprente"
  |Tr(n,Empty,Empty)-> if ((rm_from_lst n list)=[]) then n::[]
  else failwith "path_coprente non trovato"
  |Tr(n,left,right)-> 
    n::try path_coprente left (rm_from_lst n list)
    with _-> path_coprente right (rm_from_lst n list);;

type col = Rosso | Giallo | Verde | Blu;;
type 'a col_assoc = (col * 'a list) list;;

let rec colore x  (lst: 'a col_assoc)= match lst with
  []->failwith "colore non trovato"
  |(col,xlist)::rest-> if List.mem x xlist then col else colore x rest;;

let lista_colori = [(Rosso,[1;5;10;13]);(Giallo,[2;6;7;14]);(Verde,[3;9;11;15]);(Blu,[4;8;12;16])];;
(*Da pattern matching non esaustivo*)
let rec path_to ele (lst: 'a col_assoc) = function
  Empty-> []
  |Tr(f,Empty,Empty)-> 
    if f=ele then [f]
    else failwith "foglia non trovata"
  |Tr(n,Tr(ln,ll,lr),Empty)-> 
        if (colore ln lst)=(colore n lst) then failwith "stesso colore fra genitore e figlio"
        else n::path_to ele lst (Tr(ln,ll,lr))
  |Tr(n,Empty,Tr(rn,rl,rr))->
        if (colore rn lst)=(colore n lst) then failwith "stesso colore fra genitore e figlio"
        else n::path_to ele lst (Tr(rn,rl,rr))
  |Tr(n,Tr(ln,ll,lr),Tr(rn,rl,rr))->
    n::try if (colore ln lst)=(colore n lst) then failwith "stesso colore fra genitore e figlio"
        else path_to ele lst (Tr(ln,ll,lr))
      with _ -> if (colore rn lst)=(colore n lst) then failwith "stesso colore fra genitore e figlio"
        else path_to ele lst (Tr(rn,rl,rr));;

let abr = Tr((4,0),Tr((2,1),Empty,Tr((3,2),Empty,Empty)),Tr((8,3),Tr((6,4),Empty,Empty),Empty));;

let abr1 = Tr((4,0),Tr((2,1),Empty,Tr((5,2),Empty,Empty)),Tr((8,3),Tr((6,4),Empty,Empty),Empty));;

let rec abr_get_rootkey = function
  Empty-> failwith "abr_get_rootkey: Empty tree"
  |Tr((k,_),_,_)->k;;

let rec abr_get_rootval = function
  Empty-> failwith "abr_get_rootval: Empty tree"
  |Tr((_,v),_,_)->v;;

let rec abr_check t =
  let listaNodi = inorder t in
  let rec aux predK listaNodi = 
    match listaNodi with
    []->true
    |(k,v)::rest-> predK<k && aux k rest
  in aux (fst(List.hd listaNodi)) (List.tl(listaNodi));; 


let rec abr_search abr key = match abr with
  Empty-> failwith "nodo non trovato"
  |Tr((k,v),left,right)-> 
    if key=k then v else (if k<key then abr_search left key else abr_search right key);;

let rec abr_update abr pair =
  let (k,v) = pair in 
  match abr with
    Empty -> Tr((k,v),Empty,Empty)
    |Tr((k1,v1) as x,t1,t2) -> 
      if k = k1 then Tr(pair,t1,t2)
      else 
        if k < k1 then Tr(x,abr_update t1 pair, t2)
        else Tr(x,t1,abr_update t2 pair);;


let rec path p = function
  Empty-> []
  |Tr(n,left,right)-> if not(p n) then n::try path p left with _-> path p right else failwith "path";;

let pred n = n>10;;