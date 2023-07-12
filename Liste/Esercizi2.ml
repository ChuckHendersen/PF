let ultime_cifre n = 
  let valoreAssoluto = abs(n) in
  let accoppiatore x y = (x,y) in
  accoppiatore ((valoreAssoluto mod 100)/10)(valoreAssoluto mod 10);;

ultime_cifre 234;;
ultime_cifre 23;;
ultime_cifre 0;;
ultime_cifre (-123);;
ultime_cifre 7856;;
ultime_cifre (-2);;
ultime_cifre (-65);;

let cifraBella n = match n with 
 0->true
|3->true
|7->true
|_->false;;

let bello n = 
  let valoreAssoluto = abs(n) in
  if valoreAssoluto>9 then not(cifraBella((valoreAssoluto mod 100)/10))&& cifraBella(valoreAssoluto mod 10)
  else cifraBella(valoreAssoluto);;

let isDataValida (giorno,mese) = match mese with 
  "gennaio"->giorno>=1 && giorno<=31
  |"febbraio"->giorno>=1 && giorno<=28
  |"marzo"->giorno>=1 && giorno<=31
  |"aprile"->giorno>=1 && giorno<=30
  |"maggio"->giorno>=1 && giorno<=31
  |"giugno"->giorno>=1 && giorno<=30
  |"luglio"->giorno>=1 && giorno<=31
  |"agosto"->giorno>=1 && giorno<=31
  |"settembre"->giorno>=1 && giorno<=30
  |"ottobre"->giorno>=1 && giorno<=31
  |"novembre"->giorno>=1 && giorno<=30
  |"dicembre"->giorno>=1 && giorno<=31
  |_->false;;

let data (n,s) = isDataValida(n,s);;