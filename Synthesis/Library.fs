module Synthesis

open System.Security.Cryptography.X509Certificates

let abelar n =  
    n>12 &&  n<3097 && n%12=0 
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
    let rec countDigit v acc=
        match n/v=0 with
        |true->acc
        |false->countDigit (v*10) (acc+1)
    countDigit 10 1
failwith "Not implemented"

let minmax (a,b,c,d) =
    min (min a b) (min c d),max (max a b) (max c d)
failwith "Not implemented"

let isLeap n =
    match n<1582,n%4=0,n%100<>0,n%400=0 with
    |true,_,_,_->failwith "An exception has been thrown"
    |_,true,true,_->true
    |_,_,_,true->true
    |_,_,_,_->false
failwith "Not implemented"
let month n = match n with
    |1 -> "January",31
    |2-> "February",28
    |3-> "March",31
    |4-> "April",30
    |5-> "May",31
    |6-> "June",30
    |7-> "July",31
    |8-> "August",31
    |9-> "September",30
    |10-> "October",31
    |11-> "November",30
    |12-> "December",31
    |_-> failwith "An exception is thrown!!"
failwith "Not implemented"

let toBinary n =
    match n>=0,n with
     |true,0->"0"
     |false,_->failwith "A negative number has been supplied"
     |true,_->
    let rec Div v=
        match v/2<>0 ||v%2=1 with
        |false -> ""
        |true-> match v%2 with
            |0->Div (v/2)+"0"
            |1->Div(v/2)+"1"
    Div n

let bizFuzz n =
    let rec countDivs (a,b,c) v =
        match n<v with
        |true->(a,b,c)
        |false-> match v%3=0,v%5=0,v%3=0 && v%5=0 with
            |true,false,_->countDivs (a+1,b,c) (v+1)
            |false,true,_->countDivs (a,b+1,c) (v+1)
            |false,false,true->countDivs (a,b,c+1) (v+1)
            |true,true,true->countDivs (a+1,b+1,c+1) (v+1)
            |_,_,_->countDivs (a,b,c) (v+1)
    countDivs (0,0,0) 1
failwith "Not implemented"

let monthDay d y =
    match (d<=0),(d=366 && (isLeap y)=false),(d>366) with
    |true,false,_|false,true,false|false,false,true-> failwith "An exception has been thrown"
    |_,_,_->
        let rec monthCheck v numofdays= match v=13 with
          |true->"December"
          |false->
            let mon,days=month v 
            let febchecker = match v=2 && (isLeap y) with
                |true->days+1
                |false->days
            match numofdays<d with
            |true->monthCheck (v+1) (numofdays+febchecker) 
            |false->
                let mon,day=month (v-1)
                mon
        monthCheck 1 0
failwith "Not implemented"

let sqrt n=
     let rec calculate guess i=
        match i with
        |10->guess
        |_->
          let g=(guess + n/guess)/2.0
          calculate g (i+1)
     match n<=0.0 with
     |true->failwith "Can't take square root of negative"
     |_-> 
        calculate (n/2.0) 0
failwith "Not implemented"       

let coord n =
    let (x1,y1)=n
    let dist (x2,y2)= sqrt ((x1-x2)**2.0 + (y1-y2)**2.0)
    let within (x,y) width height =  match (x1<=(x+width) && x1>=x) && (y1<=y && y1>=y-height) with
                                        |true->true
                                        |_->false
    dist,within
failwith "Not implemented"