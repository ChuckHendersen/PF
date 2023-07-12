(*Filter caselle, da fare dopo aver riscritto il codice della lezione sul labirinto e il mostro*)
let mostri = [(0,2);(1,1);(1,3);(2,3);(3,0);(4,2)];;

let somma x = function y -> x+y;;

let somma x y = x+y;;

let dim = 5;;

let funzione x = function
[]->x

let funzione x lista = match lista with
[]->x

exception LunghezzeDiverseException;;

let combine aList bList =
  if List.length(aList) <> List.length(bList) then raise LunghezzeDiverseException
  else let rec aux aList bList abList =
    if aList = [] then abList
    else aux (List.tl(aList))(List.tl(bList))(abList@[(List.hd aList,List.hd bList)])
  in aux (aList)(bList)([]);;

let split abList =
  let rec aux abList (aList,bList)=
    match abList with
    []->(aList,bList)
    |(a,b)::rest->aux(rest)(aList@[a],bList@[b])
  in aux(abList)([],[]);; 

let cancella chiave abList =
  let rec aux chiave abList resList=
    match abList with
    []-> resList
    |(a,b)::rest-> if chiave <> a then aux(chiave)(rest)(resList@[(a,b)])
                  else aux (chiave)(rest)(resList)
  in aux chiave abList [];;

(*let unione a b = a@b;;*)

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
  let listaElemComuni = intersezione aList bList in
  let listaUnita = unione aList bList in
  let rec aux listaUn=
    match listaUn with
    []->[]
    |x::rest-> if (List.mem(x)(listaElemComuni)) then aux(rest)
              else x::aux(rest)
  in aux(listaUnita);;

let subset set1 set2=
  let rec aux set1 =
    match set1 with
    []->true
    |x::rest -> List.mem x set2 && aux rest in 
  aux set1;;

let explode stringa =
  let len = String.length stringa in
  let rec aux n = 
    if n<len then stringa.[n]::aux(n+1)
    else [] in
  aux(0);;

(*let implode lst = 
  let rec aux n stringa = function
  []-> stringa
  |x::rest-> String.make n x*)

let rec listMem x = function
[]-> false
|y::rest -> x=y || listMem x rest

(*let implode listChar = 
  let rec aux*)

(*let inpairs n =
  let rec aux n =
    1->[(1,1)]
    |_-> [(n,n)]::*)