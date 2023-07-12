let filquad list_num list_quad = 
  let rec aux numeri = match numeri with
    []-> []
    |x::rest-> if (List.mem (x*x) list_quad) then x::aux(rest)
                else aux(rest)
  in aux(list_num);;

let filquad_it list_num list_quad =
  let rec aux numeri res = match numeri with
    []->res
    |x::rest-> if (List.mem (x*x) list_quad) then aux(rest)(res@[x])
    else aux(rest)(res)
  in aux(list_num)([]);;

let filquad_filter list_num list_quad =
  List.filter (fun x-> List.mem (x*x) list_quad) list_num;;