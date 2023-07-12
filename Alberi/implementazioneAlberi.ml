type 'a tree = 
  Leaf of 'a
  |One of 'a * 'a tree
  |Two of 'a * 'a tree * 'a tree;;
(* Questa implementazione è molto matematica ma a noi programmatori piacciono
   cose più pratice*)

let rec size = function
  Leaf _ -> 1
  |One (_,subtree)-> 1+size(subtree)
  |Two (_,subtree1,subtree2)->1+size(subtree1)+size(subtree2);;

let foglia = Leaf(1);;
size(foglia);;

let radiceFoglia = One(2,foglia);;
size(radiceFoglia);;

let radice2Figli = Two(3,foglia,radiceFoglia);;
size(radice2Figli);;

(*Implementazione alberi alternativa*)

exception EmptyTree;;
exception NotALeaf;;

type 'a tree = 
  Empty
  |Tr of 'a * 'a tree * 'a tree;;

let is_empty = function 
  Empty-> true
  |_->false;;

let root = function
  Empty-> raise EmptyTree
  |Tr(x,_,_)->x;;

let is_leaf = function
  Tr(_,Empty,Empty)->true
  |_->false;;

let leaf = function 
  Tr(x,Empty,Empty)->x
  |_->raise NotALeaf;;

let size = function
  Empty->0
  |Tr(_,t1,t2)->1+size(t1)*size(t2);;

let right = function
 Empty-> raise EmptyTree
 |Tr(_,_,t1)-> t1;;

let left = function
 Empty-> raise EmptyTree
 |Tr(_,t2,_)-> t2;; 

let foglia = Tr(1,Empty,Empty);;
size(foglia);;

let radiceFoglia = Tr(2,foglia,Empty);;
size(radiceFoglia);;

let radice2Figli = Tr(3,foglia,radiceFoglia);;
size(radice2Figli);;


(*Molto complicato rispetto la soluzione data*)
(*let rec count tr = 
  let rec listAssocOpt k = function
    []-> failwith "no such element"
    |(k2,v)::rest-> if k=k2 then (k2,v)
                    else listAssocOpt k rest
  in
  let rec fuse listAssoc1 listAssoc2 = match listAssoc1 with
    []->[listAssoc2]
    |(k,v)::rest-> if List.mem_assoc k listAssoc2 then 
                    let kV2 = listAssocOpt k listAssoc2 in
                    (k,snd(kV2)+v)::(List.remove_assoc k listAssoc2)::(fuse rest listAssoc2)
                    else [(k,v)]::fuse(rest)(listAssoc2)
  in
  match tr with                   
  Empty -> []
  |Tr(x,tr1,tr2)->  fuse([x,1])(fuse(count tr1)(count tr2))*)

let rec add y = function
 []->[(y,1)]
 |(x,n)::rest -> if x=y then (x,n+1)::rest
                else (x,n)::add y rest

let count t =
  let rec aux result = function
    Empty-> result
    |Tr(r,tr1,tr2)-> aux (aux (add r result) tr1) tr2
  in aux [] t;; 