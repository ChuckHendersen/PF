exception LunghezzeDiverse;;

let combine aList bList=
  if List.length aList <> List.length bList then raise LunghezzeDiverse
  else let rec aux aList bList res = match aList with
  []-> List.rev res
  |x::rest -> aux(rest)(List.tl bList)((x,List.hd bList)::res)
in aux aList bList [];;

let split abList = 
  let rec aux abList res =
    match abList with
    []->res
    |(x,y)::rest-> aux rest ((fst(res)@[x],snd(res)@[y])) 
  in aux abList ([],[]);;

let remove abList aKey=
  let rec aux abList res = match abList with
  []->List.rev res
  |(x,y)::rest -> if x=aKey then aux(rest)(res)
                  else aux(rest)((x,y)::res)
  in aux abList [];; 

  let rec remove abList aKey= match abList with
  []->[]
  |(x,y)::rest -> if x=aKey then remove(rest) aKey
                  else (x,y)::remove(rest) aKey;;

    let unione a b =
  let rec aux a =
    match a with 
    []->b
    |x::rest->if(List.mem x b) then aux(rest)
              else x::aux(rest)
  in aux (a);;

let intersezione aList bList = 
  let rec aux aList = 
    match aList with
    []->[]
    |x::rest-> if (List.mem(x)(bList)) then x::aux(rest)
                else aux (rest)
  in aux aList;;

let differenza aList bList = 
  let listaUnita = unione aList bList in
  let listaIntersecata = intersezione aList bList in
  let rec aux = function
  []-> []
  |x::rest -> if List.mem x listaIntersecata then aux rest
              else x::aux(rest)
  in aux listaUnita;;

let subset aList bList =
  List.for_all (fun x-> List.mem x bList) aList;;

let explode stringa =
  let len = String.length stringa in
  let rec aux stringa i = 
    if i<len then stringa.[i]::aux stringa (i+1)
    else []
  in aux stringa 0;;

let myFlatten aListList=
  let rec aux = function
  []->[]
  |aList::rest-> aList @ aux (rest)
in aux aListList;;

let myFlatten aListList=
  let rec addLista lst1 lst2 =
    match lst1 with
    []->lst2
    |x::rest-> x::addLista(rest)(lst2) in
  let rec aux = function
    []->[]
    |aList::rest-> addLista(aList)(aux(rest))
  in aux aListList;;

let intpairs n =
  let rec creaListaUpTo = function 
  0->[]
  |x->x::creaListaUpTo(x-1) in
  let lista1 = creaListaUpTo n in
  let lista2 = creaListaUpTo n in
  myFlatten(List.map (fun x1 -> List.map (fun x2-> (x1,x2) ) lista2 ) lista1);;

let rec trips aList = 
  match aList with
  x::y::z::rest-> (x,y,z)::trips(y::z::rest)
  |_->[];;

let choose k lista = 
  let rec chooseAux k lista = 
    if k=0 || List.length lista=0 then []
    else (List.hd lista)::chooseAux(k-1)(List.tl lista)
  in 
  let rec aux k lista = match lista with
      []->[]
      |x::rest-> if List.length (x::rest)=k then chooseAux k (x::rest)::[]
                else (chooseAux k (x::rest))::aux k rest 
  in
  if k > List.length lista then failwith "lunghezza sottogruppo piu' grande della lista"
  else aux k lista;;