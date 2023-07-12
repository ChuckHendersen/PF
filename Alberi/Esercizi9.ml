type 'a ntree = Tr of 'a * 'a ntree list;;
let leaf x = Tr(x,[]);;
let t = Tr(1,[Tr(2,[Tr(3,[leaf 4;
leaf 5]);
Tr(6,[leaf 7]);
leaf 8]);
leaf 9;
Tr(10,[Tr(11,[leaf 12;
leaf 13;
leaf 14]);
leaf 15;
Tr(16,[leaf 17;
Tr(18,[leaf 19;
leaf 20])])])]);;

type multi_expr =
  MultiInt of int
  | MultiVar of string
  | MultiDiff of multi_expr * multi_expr
  | MultiDiv of multi_expr * multi_expr
  | MultiSum of multi_expr list
  | MultiMult of multi_expr list;;

let expr1 = MultiMult([MultiSum([MultiInt(4);MultiInt(5);MultiInt(9)]);MultiVar("x")])
let expr2 = MultiSum([MultiInt(4);MultiInt(5)]);;

let rec list_contains elist subelist=
  List.for_all((fun sube->List.exists(fun e-> e=sube ) elist))subelist;;

(*Questa versione non è ingrado di vedere le sotto espressioni tipo dice che 4+5 non è sotto espressione di 4+5+9*)
let rec subexpr e1 e2 = 
  e1=e2 ||
  match e1 with
  MultiInt(_)|MultiVar(_)-> false
  |MultiDiff(se1,se2)|MultiDiv(se1,se2)-> subexpr se1 e2 || subexpr se2 e2
  |MultiSum(elist)|MultiMult(elist)-> List.exists (fun x-> subexpr x e2 ) elist;;

(*________________________________________________________*)

let rec postorder = function
  Tr(n,tlist)-> aux tlist @ [n]
and aux t = match t with
  []->[]
  |t::rest-> postorder t @ aux rest;;

let rec inorder = function
  Tr(n,[])->[n]
  |Tr(n,t::rest)-> inorder t @ (n::aux rest)
and aux = function
  []->[]
  |t::rest-> inorder t @ aux rest;;

(*mia funzione di utility*)
let rec tutte_foglie = function
  Tr(f,[])->[f]
  |Tr(n,tlist)-> aux tlist
and aux = function
  []->[]
  |t::rest-> tutte_foglie t @ aux rest;;

let rec foglie_in_lista listaF = function
  Tr(f,[])-> List.mem f listaF
  |Tr(n,tlist)-> aux listaF tlist
and aux listaF = function
  []->true
  |t::rest-> foglie_in_lista listaF t && aux listaF rest;;

let rec num_di_foglie = function
  Tr(f,[])-> 1
  |Tr(n,tlist)-> aux tlist
and aux = function
  []->0
  |t::rest-> num_di_foglie t + aux rest;;

(* true *)
num_di_foglie t = List.length (tutte_foglie t);;

let l = [0;1];;

let rec listaGuida intList nt = 
  match (intList,nt) with
  ([],Tr(n,tlist))-> n
  |(g::rest,Tr(n,tlist))-> listaGuida (rest) (List.nth tlist g);;

let foglia_costo t = 
  let rec aux costo = function
    Tr(f,[])->(f,f+costo)
    |Tr(n,tlist)-> aux1 (n+costo) (n,tlist)
  and aux1 nuovoCosto = function
    (n,[])-> (n,nuovoCosto) (*il nodo n nel nuovoCosto gia c'è, per questo non viene risommato*)
    |(n,t::rest)-> 
      let nodoCosto = aux nuovoCosto t in
      let nodoCosto2 = aux1 nuovoCosto (n,rest) in
      if snd(nodoCosto) > snd(nodoCosto2) then nodoCosto else nodoCosto2
  in aux 0 t;;

let tutte_foglie_costi t =
  let rec aux costo t = match t with
    Tr(f,[])-> [(f,f+costo)]
    |Tr(n,tlist)->(n,n+costo)::(aux1 (n+costo) tlist)
  and aux1 nuovoCosto = function
    []->[]
    |t::rest-> aux nuovoCosto t @ aux1 nuovoCosto rest
  in aux 0 t;;

let rec ramo_da_lista t listaCammino k = match t with
  Tr(f,[])-> if k=f then [f] else failwith "wrong leaf"
  |Tr(n,tlist)-> 
    if List.mem n listaCammino then n::(aux listaCammino k tlist)
    else failwith "n not in listaCammino"
and aux listaCammino k = function
  []-> failwith "ramo_da_lista"
  |t::rest-> try ramo_da_lista t listaCammino k with _-> aux listaCammino k rest;;

let is_primo n =
  if n<0 then failwith "is_primo"
  else
    let divisore = n/2 in
    let rec aux i =
      if i<=divisore then (n mod i != 0) && aux (i+1)
      else not(n=1)
  in aux 2;; 
    
let rec ramo_di_primi = function
  Tr(f,[])-> if is_primo f then f else failwith "ramo_di_primi"
  |Tr(n,tlist)-> if is_primo n then aux tlist else failwith "ramo_di_primi"
and aux = function
  []-> failwith "ramo_di_primi non trovato"
  |t::rest-> try ramo_di_primi t with _ -> aux rest;;

let rec path_non_pred p = function
  Tr(f,[])-> if not(p f) then [f] else failwith "path_non_pred"
  |Tr(n,tlist)-> if not(p n) then n::aux p tlist else failwith "path_non_pred"
and aux p = function
  []-> failwith "path_non_pred"
  |t::rest-> try path_non_pred p t with _ -> aux p rest;;

let parita' n = n mod 2 = 0;;

let rec same_structure a b = match (a,b) with
  (Tr(fa,[]),Tr(fb,[]))->true
  |(Tr(_,atlist),Tr(_,btlist))->(List.length atlist)=(List.length btlist) && aux atlist btlist
and aux atlist btlist = match (atlist,btlist) with
  ([],[])->true
  |(a::arest,b::brest)-> same_structure a b &&  aux arest brest
  |_->false;;

type col = Rosso | Giallo | Verde | Blu;;
type 'a col_assoc = (col * 'a list) list;;

let rec colore x  (lst: 'a col_assoc)= match lst with
  []->failwith "colore non trovato"
  |(col,xlist)::rest-> if List.mem x xlist then col else colore x rest;;

let ramo_colorato e (listaColori: 'a col_assoc)  t =
  let rec aux primoAvvio colorePrec t =
      match t with
      Tr(f,[])-> 
        if primoAvvio then 
          if f=e then [f] 
          else failwith "ramo_colorato" 
        else 
          if colorePrec!=(colore f listaColori) && f=e then [f] 
          else failwith "ramo_colorato"
      |Tr(n,tlist)-> 
        let coloreAttuale = colore n listaColori in
        if primoAvvio then 
          n::aux1 coloreAttuale tlist
        else
          if colorePrec!=coloreAttuale then n::aux1 coloreAttuale tlist 
          else failwith "ramo_colorato"
  and aux1 (colorePrec:col) = function
  []->failwith "ramo_colorato: elemento foglia non trovato"
  |t::rest-> try aux false colorePrec t with _ -> aux1 colorePrec rest
  in aux true Rosso t;;

let listaColori = [(Rosso,[1;9;6;15;16;13]);(Giallo,[10;8;7;5;18]);(Verde,[2;17;4;12;19;20]);(Blu,[5;3;11;14])];;

let rec path_non_pred p = function
  Tr(f,[])-> if not(p f) then [f] else failwith "path_non_pred"
  |Tr(n,tlist)-> if not(p n) then n::(aux p tlist) else failwith "path_non_pred"
and aux p = function
  []->failwith "path_non_pred"
  |t::rest-> try path_non_pred p t with _-> aux p rest;;