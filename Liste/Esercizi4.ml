let rec length lista = match lista with
[]->0
|x::rest -> 1+length(rest);;

let lengthIterativo lista = 
  let rec aux lst n =
    match lst with
    []->n
    |x::rest-> aux (rest)(n+1)
  in aux lista 0;;

let rec sumOf intList = match intList with
[]->0
|x::rest->x+sumOf(rest);;

let sumOfIter intList =
  let rec aux intList res =
    match intList with
      []->res
      |x::rest-> aux(rest)(res+x)
  in aux intList 0;;

exception ListaVuota;;
(*E' gia una sorta di tail recursion, giusto?*)
let maxList lista = 
let confronto v1 v2 = if v1>v2 then v1
else v2 in
match lista with
[]->raise ListaVuota
|x::rest-> let rec aux lista max=
              match lista with
              []-> max
              |x::[]-> confronto x max
              |x::rest-> confronto x (aux rest max)
            in aux lista x;;
(*Ora funziona, che fatica per farla funzionare*)

(*Intrinsecamente tail recursive*)
let drop n lst = 
  let rec loop n lst = 
    if n>= List.length lst then []
    else 
      match n with 
        0->lst
        |n-> loop (n-1) (List.tl lst)
  in loop n lst ;;

let append lst1 lst2 = 
  let rec aux lst1 lst2 =
    match lst1 with
    []->lst2
    |x::[]->x::lst2
    |x::rest -> x::aux(rest)(lst2)
  in aux lst1 lst2;;

(* Come cavolo la faccio tail recursive senza che la lista venga al contrario?
   let appendIter lst1 lst2*)

let reverse lst = 
  let rec aux lst revList= match lst with
    []->revList
    |x::[]->x::revList
    |x::rest-> aux (rest) (x::revList)
  in aux lst [];;

exception InvalidIndex;;

let nth n lst = 
  if n<0 || n>List.length lst then raise InvalidIndex
  else let rec aux n lst = match n with
        0-> List.hd lst
        |n-> aux (n-1) (List.tl lst)
      in aux n lst;;

let remove item lst = 
  let rec aux item lst newList = 
    match lst with
    []-> reverse(newList)
    |x::[]-> if (x=item) then reverse(newList)
              else reverse(x::newList)
    |x::rest-> if x=item then aux(item)(rest)(newList)
              else aux(item)(rest)(x::newList)
  in aux(item)(lst)([]);;

let remove2 item lst = 
  let rec aux item lst newList = 
      match lst with
      []-> newList
      |x::[]-> if (x=item) then newList
                else newList @ x::[]
      |x::rest-> if x=item then aux(item)(rest)(newList)
                else aux(item)(rest)(newList @ x::[])
    in aux(item)(lst)([]);;

remove 2 [2;3;5];;
remove 4 [2;3;5];;
remove 1 [];;
remove 2 [2];;

remove2 2 [2;3;5];;
remove2 4 [2;3;5];;
remove2 1 [];;
remove2 2 [2];;

let remove3 x list = 
  let rec aux list res = match list with
  []->res
  |y::rest->if(y=x)then aux (rest) (res)
  else aux rest (res @ [y])
in aux list [];;

remove3 2 [2;3;5];;
remove3 4 [2;3;5];;
remove3 1 [];;
remove3 2 [2];;

let copy n x = 
  let rec aux n x lst =
    match n with
    0->lst
    |n-> aux(n-1)(x)(x::lst)
  in aux n x [];;

(* Determinare il valore e il tipo di copy 3 (copy 2 8):
   Tipo: int-> int list -> int list list (alla fine verrà una lista di liste)
   Valore: [[8;8];[8;8];[8;8]] *)

let nondec intList =
  let rec aux intList = match intList with
    []->true
    |x::[]->true
    |x::rest-> if x < List.hd rest then (true && aux(rest))
              else false
  in aux intList;;

nondec [1;2;5];; (*true*)
nondec [2;4;3];; (*false*)
nondec [];; (*true*)
nondec [2];; (*true*)
nondec [1;10];; (*true*)

let pairwith a lst =
  let rec aux a lst newLst=
    match lst with
    []->newLst
    |x::[]-> (a,x)::newLst
    |x::rest-> (a,x)::aux a rest newLst
  in aux a lst [];;

let duplica lst =
  let rec aux lst newLst =
    match lst with
    []->newLst
    |x::[]-> x::(x::newLst)
    |x::rest-> x::(x::aux(rest)(newLst))
  in aux lst [];;

let enumera lst = 
  let rec aux n lst newLst =
    match lst with
    []-> newLst
    |x::[]-> (n,x)::newLst
    |x::rest-> (n,x)::aux(n+1)(rest)(newLst)
  in aux 0 lst [];;

enumera [2];;
enumera [2;3;5;23];;
enumera [1;32];;
enumera [];;

exception ItemNotFoundException;;

let position item list =
  let rec aux item list pos =
    match list with
    []-> raise ItemNotFoundException
    |x::rest-> if x=item then pos
              else aux item rest (pos+1)
  in aux item list 0;;

let alternate list =
  let rec aux list n newList =
    match list with 
    []->newList
    |x::rest-> if (n mod 2)=1 then x::(aux(rest)(n+1)(newList))
              else aux(rest)(n+1)(newList)
  in aux list 0 [];;

  let minList lista = 
    let confronto v1 v2 = if v1<v2 then v1
    else v2 in
    match lista with
    []->raise ListaVuota
    |x::rest-> let rec aux lista max=
                  match lista with
                  []-> max
                  |x::[]-> confronto x max
                  |x::rest-> confronto x (aux rest max)
                in aux lista x;;

(*Si sfrutta l'implementazione del maxList fatta precedentemente e la si ripete anche per minList*)
let min_dei_max intlistList = 
  let rec aux listList listaMax=
    match listList with
    []->minList(listaMax)
    |x::rest-> aux(rest)((maxList(x))::listaMax)
  in aux intlistList [];;

min_dei_max [[4;5];[0;99];[2;3]];; (*Ris: 3*)

(*let split2 lista =*) 