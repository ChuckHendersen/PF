let partition p lista =
  let lista =  List.map(fun x -> (p x, x)) lista in
  let rec aux lista yn= 1
  in aux lista ([],[])

(*let interleave intero lista =
  let rec aux lista = match list with
  []->
  |x::rest-> intero::(x::rest)::*)