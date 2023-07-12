(*Facciamo la funzione sommatore che prende come parametro il limite inferiore, il superiore e la funzione da applicare per ogni valore*)

let rec sum f lower upper = 
  if lower=upper then 0
  else f lower + sum (f)(lower+1)(upper);;
(*F è una funzione, per fare una versione dove la sommatoria fa semplicemente la somma di tutti i numeri fra lower e upper, 
allora f è una funzione identità cioé f(x)=x*)
sum (fun x->x) 0 10;;  