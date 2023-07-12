let isOra ora = ora>=0 && ora <=23;;
let isMinuto minuto = minuto>=0&&minuto<=59;;

exception NotaTimeException;;

let somma_ore (o1,m1) (o2,m2) = 
  let sommaMinuti = m1+m2 in
  let sommaOre = o1+o2 in
  if isOra o1 && isOra o2 && isMinuto m1 && isMinuto m2 then
   ((sommaOre+(sommaMinuti/60))mod 24,(sommaMinuti)mod 60)
  else raise NotaTimeException;;

exception NotANumber;;

let isNumero c = c>='0' && c<='9';;

(*let read_max() non li so fare*)

let read_max() = 
  try
    let rec aux (res) =
      let stringa = read_line() in
      match stringa with 
      " "-> res
      | _ -> max res (aux(int_of_string(stringa)))
    in aux (int_of_string (read_line()))
  with _ -> failwith "Inserito elemento non numerico";;

  let read_max() = 
  try
    let rec aux max =
    try
      let numero = read_int() in
        if numero>max then aux(numero)
        else aux(max)
    with _->max
    in aux (read_int())
  with _ -> failwith "Inserito elemento non numerico";;

(*let read_max_min() = *)




let sumbetween n m = 
  let rec aux n m res= 
    if n>m then res
    else aux(n+1) m (res+n)
  in aux n m 0;;

let sumto n = 
  let res = 0 in
  let altraVariabile = "ciao" in
  let rec aux n i =
    if i>n then res
    else aux n (i+1) (res+i)
  in aux n 0;;

let power n k =
  let rec aux i res = 
    if i>k then res
    else aux (i+1) (res*n)
  in aux 1 1;;

let fib n =
  let rec aux n=
    if n = 0 then 0
    else if n = 1 then 1
    else aux(n-1) + aux(n-2)
  in aux n;;

exception EmptyString;;

let max_string stringa = 
  if String.length stringa = 0 then raise EmptyString
  else 
    let l = String.length stringa in
    let rec aux maxChar i = 
      if i>=l then maxChar
      else 
        if maxChar<stringa.[i] then aux (stringa.[i]) (i+1)
        else aux maxChar (i+1)
    in aux (stringa.[0]) 0;;