module Synthesis

let abelar n =  n>12 &&  n<3097 && n%12=0 
failwith "Not implemented"

let area bases height = 
    match bases<0.0 || height<0.0 with
    |true->failwith "An exception is thrown either of the base or height are negative"
    |false-> 0.5*bases*height


let zollo n =
    match n<0 with
    |true->n*(-1)
    |_->2*n
failwith "Not implemented"

let min a b =
   match a<b with
   |true->a
   |false->b
failwith "Not implemented"

let max a b =
    match a<b with
    |true->b
    |false->a
failwith "Not implemented"

let ofTime h m s = 
    (3600*h)+60*m+s
failwith "Not implemented"

let toTime secs =
    let hours= secs/3600
    let mins=(secs%3600)/60
    let seconds= (secs%60)
    match hours<0||mins<0||seconds<0 with
    |true->0,0,0
    |false-> hours,mins,seconds
failwith "Not implemented"

let digits n =
    let start=10
    let rec countDigit v acc=
        match n/v=0 with
        |true->acc
        |false->countDigit (v*10) (acc+1)
    countDigit start 1
failwith "Not implemented"

let minmax (a,b,c,d) =min (min a b) (min c d),max (max a b) (max c d)
failwith "Not implemented"

let isLeap n =
    match n<1582 with
    |true->failwith "An exception has been thrown"
    |false->(n%4=0)&&(n%100=0)||(n%400=0)

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"