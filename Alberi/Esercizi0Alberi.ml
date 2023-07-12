type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

let rec path t foglia = 
  match t with
  Empty -> failwith "percorso non trovato"
  |Tr(x,Empty,Empty)-> if x=foglia then [x]
                      else failwith "percorso non trovato"
  |Tr(x,left,right)-> x::try percorso left tappa foglia 
                        with _-> percorso right tappa foglia


let rec visitaAlbero = function
  Empty->[]
  |Tr(foglia,Empty,Empty)-> [foglia]
  |Tr(r,tr1,tr2)-> [r] @ visitaAlbero tr1 @ visitaAlbero tr2;;

let rec sommaPesi = function
  Empty->[]
  |Tr(foglia,Empty,Empty) -> [foglia]
  |Tr(r,tr1,tr2) ->  (List.map (fun x->x+r) (sommaPesi(tr1))) @ (List.map (fun y-> y+r) (sommaPesi(tr2)))

let rec sommaPesi=
 Empty-> []

 |Tr(r,tr1,tr2)-> r+