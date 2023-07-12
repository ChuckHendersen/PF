let rec upto inf sup =
  if inf>sup then []
  else inf::upto (inf+1)(sup)

let rec conta n lista = 
  if lista=[] then 0
  else if n = List.hd (lista) then 1+conta n (List.tl (lista))
       else conta n (List.tl (lista))  
