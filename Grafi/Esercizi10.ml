type 'a graph = ('a * 'a) list;;
let grafo = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6);(5,4);(6,5);(6,7)];;
let grafo_1 = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6);(5,4);(6,5);(6,7);(2,3)];;

(*grafi orientati*)
let rec successori nodo = function
  []->[]
  |(x,y)::rest-> if x=nodo then y::successori nodo rest else successori nodo rest;;

(*grafi non orientati*)
let rec vicini nodo = function
  []->[]
  |(x,y)::rest-> 
    if x=nodo then y:: vicini nodo rest 
    else if y=nodo then x::vicini nodo rest 
    else vicini nodo rest;;
  
let test_connessi grafo n m=
  let rec aux visited nodo = 
    if List.mem nodo visited then failwith "ciclo"
    else nodo = m || auxlist (nodo::visited) (successori nodo grafo)
  and auxlist visited = function
    []-> failwith "ciclo"
    |x::rest-> 
      try aux visited x
      with _ -> auxlist (x::visited) rest
  in aux [] n;;

let esiste_ciclo grafo n = 
  let rec aux visited nodo = 
    (*mettendo il "nodo=n" quà su si evita il falso risultato perchè se voglio verificare
       che ci sia un ciclo sul nodo di partenza, allora quel nodo sarà in visited e l'eccezione
       verrebbe sempre sollevata e quindi il risultato sarebbe sempre falso*)
    nodo = n || if List.mem nodo visited then failwith "gia visitato"
    else auxlist (nodo::visited) (vicini nodo grafo)
  and auxlist visited = function
    []-> false
    |x::rest-> 
      try aux visited x
      with _-> auxlist (x::visited) rest
  in
  (*si utilizza la routine init perchè non ha senso controllare che nodo 
     sia uguale a n se sarà per forza così essendo il nodo di partenza*)
  let init visited nodo =
    auxlist (nodo::visited) (successori nodo grafo)
  in
  init [] n;;

let ciclo g start = 
  let rec aux visited nodo =
    if nodo=start then [nodo]
    else 
      if List.mem nodo visited then failwith "path"
      else nodo::auxlist (nodo::visited) (successori nodo g)
  and auxlist visited = function
    []->failwith "nessun ciclo"
    |x::rest-> 
      try aux visited x
      with _-> auxlist visited rest
  in
  let init nodoPartenza = 
    nodoPartenza::auxlist ([nodoPartenza]) (successori nodoPartenza g)
  in init start;;


type 'a graph1 = ('a * 'a list) list;;
let grafo1 = [(1,[2;3;4]); (2,[6]); (3,[5]); (4,[6]); (5,[4]); (6,[5;7]); (7,[])];;

let rec successori1 nodo (g: 'a graph1) = match g with
  []->[]
  |(n,listaArchi)::rest-> if n=nodo then listaArchi else successori1 nodo rest;;

let rec vicini1 nodo (g: 'a graph1) = match g with
  []->[]
  |(n,listaArchi)::rest-> if n=nodo then listaArchi @ vicini1 nodo rest
  else if List.mem nodo listaArchi then n::vicini1 nodo rest
  else vicini1 nodo rest;;

(*SOLUZIONE DELLA PROF*)
type 'a graph2 = 'a list * ('a * 'a) list;;
(* cerca: 'a list -> 'a list -> bool *)
(* cerca visited listanodi = true se da uno dei nodi in listanodi
      si puo' raggiungere goal senza passare per nodi di visited *)
let raggiungibile archi start goal =
  let rec cerca visited = function
    [] -> false
    | n::rest ->
      if List.mem n visited
      then cerca visited rest
      else n=goal ||
        cerca (n::visited)
          ((vicini n archi)@rest)
  in cerca [] [start];;
      
(* grafo_connesso : 'a graph2 -> bool *)
(* aux: 'a list -> bool
  aux listanodi = true se da ciascun nodo in listanodi e' raggiungibile
  il nodo che lo segue in listanodi stessa *)
let grafo_connesso (nodi,archi) =
  let rec aux = function
    [] | [_] -> true
    | n::m::rest -> raggiungibile archi n m && aux (m::rest)
  in aux nodi;;

(*Per tutto il tempo lo stavo facendo su una struttura dati sbagliata porco dio*)

type 'a graph2 = 'a list * ('a * 'a) list;;
let grafo2 = ([1;2;3;4;5;6;7],[(1,2);(2,6);(6,7);(6,5);(4,6);(5,4);(1,3);(3,5);(1,4)]);;
let grafo2_1 = ([1;2;3],[(1,2);(2,3);(3,2);(3,1)]);;

let rec vicini2 nodo (nodi,archi) = match archi with
  []->[]
  |(n,m)::rest-> if nodo=n then m::successori2 nodo (nodi,rest) else if nodo=m then n::successori2 nodo (nodi,rest) else successori2 nodo (nodi,rest);;

let rec successori2 nodo (nodi,archi) = match archi with
  []->[]
  |(n,m)::rest-> if nodo=n then m::successori2 nodo (nodi,rest) else  successori2 nodo (nodi,rest);;

let rec rimuovi_da_lista e = function
  []->failwith "elemento non trovato"
  |x::rest-> if e=x then rest else x::rimuovi_da_lista e rest;;

let cammino grafo2 listaNodi start goal =
  let rec aux listaNod visited nodo =
    try 
      let nuovaListaNodi = rimuovi_da_lista nodo listaNod in
      if nodo = goal then 
        if nuovaListaNodi=[] then [nodo]
        else failwith "cammino"
      else 
        if List.mem nodo visited then failwith "cammino"
        else nodo::(auxlist nuovaListaNodi (nodo::visited) (successori2 nodo grafo2))
    with _-> failwith "cammino"
  and auxlist listaNod visited = function
        []->failwith "cammino"
        |x::rest-> try aux listaNod visited x
          with _-> auxlist listaNod visited rest
  in aux listaNodi [] start;;

let hamiltoniano g =
  let start = List.hd (fst(g))
  in
  let rec aux visited nodo =
    if nodo = start then
      (*if (List.length visited) <> 0 then*)
        if (List.length visited) = (List.length (fst(g))) then [nodo]
        else failwith "non hamilt"
      (*else
        nodo::(auxlist (nodo::visited) (successori2 nodo g))*)
    else 
      if List.mem nodo visited then failwith "path"
      else nodo::(auxlist (nodo::visited) (successori2 nodo g))
  and auxlist visited = function
        []-> failwith "path"
        |x::rest->
          try aux visited x
          with _-> auxlist visited rest
  in (*aux [] start;;*)
  let init visited primoNodo =
    primoNodo::(auxlist (primoNodo::visited) (successori2 primoNodo g))
  in init [] start;;

type col = Rosso | Giallo | Verde | Blu;;
type 'a col_assoc = (col * 'a list) list;;

let lista_colori_nodi = [([1;5],Rosso);([2;6],Giallo);([3;7],Verde);([4],Blu)];;

let rec colore_del_nodo nodo = function
  []->failwith "lista non valida"
  |(listaNodi,colore)::rest-> 
    if List.mem nodo listaNodi then colore 
    else colore_del_nodo nodo rest;;

let colori_alterni grafo lcn start goal =
  let rec aux visited nodo =
    if (List.mem nodo visited) then failwith "path"
    else
      if nodo=goal then [nodo]
      else nodo::auxlist(nodo::visited)(colore_del_nodo nodo lcn)(successori nodo grafo)
  and auxlist visited colore_nodo_prec = function
    []-> failwith "path1"
    |x::rest->
      try 
        if colore_nodo_prec = (colore_del_nodo x lcn) then failwith "stesso colore"
        else aux visited x
      with _-> auxlist visited colore_nodo_prec rest
  in aux [] start;;

let lista_grafi = [grafo;grafo_1];;

let connessi_in_glist listaGrafi b c =
  let rec aux grafo visited nodo =
    if List.mem nodo visited then failwith "path"
    else
      nodo=c || auxlist grafo (nodo::visited) (successori nodo grafo)
  and auxlist grafo visited = function
        []->failwith "path1"
        |x::rest->
          try aux grafo visited x
          with _-> auxlist grafo visited rest
  in
  let init grafo start =
    auxlist (grafo) ([]) (successori start grafo)
  in
  let rec applica = function
    []-> false
    |g::rest-> 
      try init g b
      with _-> (false || applica rest)
  in applica listaGrafi;;
  
let rec rimuovi_da_lista_1 e = function
  []->[]
  |x::rest-> if e=x then rest else x::rimuovi_da_lista_1 e rest;;

let cammino_con_nodi grafo listaNodi start goal =
  let rec aux listaNod visited nodo =
      let nuovaListaNodi = rimuovi_da_lista_1 nodo listaNod in
      if nodo = goal then 
        if nuovaListaNodi=[] then [nodo]
        else failwith "cammino"
      else 
        if List.mem nodo visited then failwith "cammino"
        else nodo::(auxlist nuovaListaNodi (nodo::visited) (successori nodo grafo))
  and auxlist listaNod visited = function
        []->failwith "cammino"
        |x::rest-> try aux listaNod visited x
          with _-> auxlist listaNod visited rest
  in aux listaNodi [] start;;

let path_n_p g p n start =
  let rec aux num visited nodo=
    if List.mem nodo visited then failwith "path"
    else 
      if p nodo then
        if num=0 then [nodo]
        else nodo::auxlist (num-1) (nodo::visited) (successori nodo grafo)
      else nodo::auxlist (num-1) (nodo::visited) (successori nodo grafo)
  and auxlist num visited = function
    []-> failwith "path_n_p"
    |x::rest-> try aux num visited x
      with _-> auxlist num visited rest
  in aux n [] start;;
(*path_n_p : 'a -> (int -> bool) -> int -> int -> int list = <fun>* PERCHE' da int ovunque?*)

let tutti_cammini g start goal =
  let rec aux visited nodo=
    if List.mem nodo visited then []
    else 
      if nodo=goal then [List.rev (nodo::visited)]
      else auxlist (nodo::visited) (successori nodo g)
  and auxlist visited = function
    []->[]
    |x::rest-> (aux visited x) @ auxlist visited rest
  in aux [] start;;

let visita g start =
  let rec aux visited toBeVisited = match toBeVisited with
    []->List.rev visited
    |x::rest-> if List.mem x visited then aux visited rest
      else aux (x::visited) (rest @ (successori x g)) 
      
  in aux [] [start];;


let visita_profondita g start =
  let rec aux visited = function
    []->[]
    |x::rest-> 
      if List.mem x visited then aux visited rest (* se x è gia stato visitato non è necessario aggiungere a rest i suoi vicini/successori perchè il passaggio è già stato fatto quando non era stato visitato*)
      else x::aux (x::visited) ((successori x g)@rest) (*se perdiamo di vista rest in sostanza sovrascriverei sempre la lista di nodi da visitare in generale, Prenderebbe sempre il primo della lista di successori.*)
  in aux [] [start];;

let visita_ampiezza g start =
  let rec aux visited = function
    []->[]
    |x::rest-> 
      if List.mem x visited then aux visited rest
      else x::aux(x::visited)(rest@(successori x g))
  in aux [] [start];;
  
    
  