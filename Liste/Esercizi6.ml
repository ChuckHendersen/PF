let rec find p = function
[]-> failwith "elemento non trovato"
|x::rest-> if p x then x
          else find p rest;;

let rec takewhile p = function
[]->[]
|x::rest -> if p x then x::takewhile p (rest)
            else [];;

let rec dropwhile p = function
[]->[]
|x::rest -> if p x then dropwhile p rest
            else x::rest;;

let rec partition p lista = 
  let rec aux lista yList nList=
  match lista with
  []-> (List.rev yList, List.rev nList)
  |x::rest-> if p x then aux rest (x::yList)nList
              else aux rest yList(x::nList)
  in aux lista [] [];;

let pairwith a list = List.map (fun x-> (a,x)) list;;

let verifica_matrice n matrice = List.exists( List.for_all(fun x->x<n)) matrice;;

let setdiff list1 list2 =
  List.filter ((fun x->not (List.exists (fun y-> x=y)list2) )) list1
  @
  List.filter ((fun x->not (List.exists (fun y-> x=y)list1) )) list2;;

let setdiffv2 list1 list2 =
  List.filter ((fun x->not (List.mem x list2) )) list1
  @
  List.filter ((fun x->not (List.mem x list1) )) list2;;

let subset list1 list2 =
  List.for_all(fun x-> List.mem x list2 )list1;;

let duplica intList= List.map (fun x-> x*2) intList;;

let mapcons ablistList elem = 
  List.map (fun (a,b)-> (a,elem::b)) ablistList;;

let tutte_liste_con n x y =
  let rec aux n res=
  if n = 0 then res
  else aux (n-1) (List.map (fun j-> x::j) res @ List.map (fun k-> y::k) res)
  in aux n [[]]

let rec interleave n = function 
  []->[[n]] 
  |x::rest-> (n::x::rest):: List.map (fun y-> x::y) (interleave n rest)

let rec permut = function 
[]->[[]]
|x::rest->List.flatten(List.map(interleave x) (permut rest))