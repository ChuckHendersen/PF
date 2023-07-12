let aux y z = (y,z);;

let ultimiDue x = aux ((abs(x) mod 100)/10) (abs (x) mod 10);;

ultimiDue 123;;

ultimiDue 5;;

ultimiDue 23412;;

ultimiDue 0;;

ultimiDue 1;;

let isBello n = n=0 or n=3 or n=7;;

let first (a,b) = a;;

let second (a,b) = b;;

let bello n = isBello(second(ultimiDue(n))) &&
  not (isBello(first(ultimiDue(n))));; 
(*Da falso negativo con n = 7 perch� ultimiDue(n) da (0,7) come coppia
e 0 risulta essere bello*)