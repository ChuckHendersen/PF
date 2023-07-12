type 'a graph = ('a*'a) list;;

(*Successori di un certo nodo
  successori 'a -> ('a * 'a) list -> 'a list *)
(*per grafi orientati*)
let rec successori nodo = function
  []->[]
  |(x,y)::rest->if x=nodo then y::successori nodo rest else successori nodo rest;;

(*per grafi non orientati*)
let rec vicini nodo = function
  []->
  |(x,y)::rest-> if x=nodo then y::vicini nodo rest
  else y=nodo then x::vicini nodo rest
  else vicini nodo rest;;

let setadd x set = if List.mem x set then set else x::set;;

let rec nodes = function
  []->[]      (*Addizione insiemistica per evitare duplicati*)
  |(x,y)::rest->setadd x (setadd y (nodes rest));;

(* ('a->bool)->'a graph->'a->'a*)
let cerca p g start = 
  let rec aux visited = function
    []-> failwith "cerca"
    |x::rest-> 
      if List.mem x visited then aux visited rest
      else if p x then x 
      else aux (x::visited) (rest@(vicini x g))
  in aux [] [start];;

(* ('a->bool)->'a graph->'a->'a list *)
(*Nodi raggiungibili che rispettano il predicato*)
let nodi_raggiungibili p g start = 
  let rec aux visited = function
      []-> []
      |x::rest-> 
        if List.mem x visited then aux visited rest
        else if p x then x::aux(x::visited) (rest@(vicini x g))
        else aux (x::visited) (rest@(vicini x g))
    in aux [] [start];;

(*'a graph->'a->'a->'a list*)
let path g start goal = 
  let rec aux visited nodo =
    if List.mem nodo visited then failwith "path"
    else if nodo = goal then [nodo]
        else nodo::auxlist (nodo::visited) (vicini nodo g)
  and auxlist visited = function
    []-> failwith "path"
    |x::rest-> 
      try aux visited x 
      with _-> auxlist (x::visited) rest
  in aux [] start;;

let path((grafo,contenuti): 'a labirinto) ingresso uscita =
  let rec danodo visited oggetti n =
    if List.mem n visited || has_monster n contenuti then failwith "path"
    else if n=uscita then List.rev (n::visited)
    else dalista (n::visited) ((contenuto n contenuti)@oggetti) (vicini n grafo)
  and dalista visited oggetti = function
    []->failwith "qc"
    |n::rest-> 
      try danodo visited oggetti nodes
      with _-> dalista visited oggetti rest
  in danodo [] [] ingresso

(*altra versione*)
let path((grafo,contenuti): 'a labirinto) ingresso uscita =
  let rec danodo visited oggetti n =
    if List.mem n visited || has_monster n contenuti then failwith "path"
    else if n=uscita then ([n],contenuto n contenuti)
    else 
      let (path,roba) = dalista (n::visited) (vicini n grafo)
      in (n::path,(contenuto n contenuti)@roba)
  and dalista visited oggetti = function
    []->failwith "qc"
    |n::rest-> 
      try danodo visited oggetti nodes
      with _-> dalista (n::visited) oggetti rest
  in danodo [] ingresso

(*febbraio 2012*)

type shops = (int * string list) list;;
type city = (int * int) list * shops;;

let path tobuy (graph,shops) start =
  let rec from_node visited tobuy a =
    if List.mem a visited then raise NotFound
    else let nuova = diff tobuy (venduti a shops) in
      if nuova = [] then [a]
    else a::from_list (a::visited) nuova (vicini a graph)
  and from_list visited tobuy = function
   []-> raise NotFound
   |a::rest->try from_node visited tobuy a
  with NotFound -> from_list visited tobuy rest
in from_node [] tobuy start;;

esercizio 6

let cammino grafo lista start goal = 
  let rec aux lista nodo =
    if not (List.mem nodo lista) then failwith "fail"
    else
      let nuova = remove nodo lista in
      if nodo = goal 
        then
        if lista=[nodo]
        then [nodo]
        else failwith "fail"
      else nodo::aux2 nuova (successori nodo grafo)
  and aux2 l1 = function
    []-> failwith "fail"
    |x::rest-> 
      try aux x l1
    with _-> aux2 l1 rest
  in aux [] lista start