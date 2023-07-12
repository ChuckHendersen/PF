let numeric c = c>='0' && c<='9';;
let conta_digits s =
  let rec loop a i =
    try if (numeric(s.[i])) then loop(a+1)(i+1)
        else loop (a)(i+1)
    with _->a
  in loop 0 0;;
