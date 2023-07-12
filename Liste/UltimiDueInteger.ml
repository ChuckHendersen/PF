let aux y z = (y,z);;

let ultimiDue x = aux ((abs(x) mod 100)/10) (abs (x) mod 10);;

ultimiDue 123;;

ultimiDue 5;;

ultimiDue 23412;;

ultimiDue 0;;

ultimiDue 1;;


let has_range lista k =
  let rec aux lista = match lista with
  []->false
  |x::rest->if ((k>=fst(x)) && (k<=snd(x))) then true
          else aux(rest)
  in aux (lista);;

let compito ranges lista=
  let rec aux lista = match lista with
  []->[]
  |x::rest-> if has_range(ranges)(x) then x:: aux(rest)
            else aux(rest)
          in aux (lista);;