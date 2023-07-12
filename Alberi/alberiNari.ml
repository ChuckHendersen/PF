type 'a ntree = Tr of 'a * 'a ntree list;;

let rec size = function
  Tr(_,tlist)-> 1+ List.fold_left (+) 0 (List.map size tlist);;

let rec size (Tr(x,tlist))=
  1+
  match tlist with
  []->0
  |t::rest-> size (t) + size (Tr(x,rest));;

let rec size (Tr(_,tlist))=
  1+sizelist tlist 
  and sizelist = function
  []->0
  |t::rest-> size t + sizelist rest;;

(*let rec preorder = function
  Tr(x,[])->[x]
  |Tr(x,tlist)-> x:: preorder*)

let rec treemem t a= match t with
  Tr(x,[]) -> x=a (*Questo caso collassa in List.exists p [] che darebbe false comunque*)
  |Tr(x,tlist) -> (x=a) || (List.exists (fun t-> treemem t a) tlist)
