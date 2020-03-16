module Synthesis

let abelar x =
    (x>12) && (x % 12 = 0) && (x < 3097)


let area x y =
    match x<0.0 || y < 0.0 with 
        |true -> failwith "input is negative"
        |false -> 0.5*x*y
 

let zollo x =
    match x < 0 with 
        |true -> x*(-1)
        |false -> x*2
    

let min sml lil =
    match sml<lil with
    |true -> sml
    | _ -> lil
    

let max big long =
    match big > long with 
    |true -> big
    | _ -> long
    

let ofTime hrs min sec = (hrs*3600)+(min*60)+(sec)

let toTime a =
    let hr = (a%86400)/3600
    let min =(a%3600)/60
    let sec = (a%60)
    match a<=0 with 
    |true -> (0,0,0)
    |_ -> (hr, min, sec)
    


let digits x =
    let rec count i acc =
        match (x>=i)||(x<=(-i)) with
            |false -> acc
            |_ -> count (i*10) (acc+1)
    count 10 1
    

let minmax (a,b,c,d) =  
    min a b |> min c |> min d, max a b |> max c |> max d
    

let isLeap x =
    match x < 1582 with 
    |true -> failwith "year is less than 1582"
    |false -> 
    match (x%4=0 && x%100<>0) || (x%4=0 && x%100=0 && x%400=0)   with
    |true -> true
    |false -> false

let month x = 
  match x  with
  |1->("January", 31)
  |2-> ("February", 28)
  |3-> ("March", 31)
  |4->   ("April", 30)
  |5-> ("May", 31)
  |6->   ("June", 30)
  |7-> ("July", 31)
  |8-> ("August", 31)
  |9->   ("September", 30)
  |10-> ("October", 31)
  |11-> ("November", 30)
  |12->("December", 31)
  |_ ->failwith "Not implemented" 

let toBinary binCov =
    match binCov < 0 with
    |true -> failwith "not allowed"
    |false -> 
    let rec Cov z acc =
        match z = 0 with 
        |true ->
            match acc with 
            |""->"0"
            |_ ->acc
        |false -> 
            let back = z % 2
            match back = 0 with 
            |true ->Cov(z/2) ("0"+acc)
            |false -> Cov(z/2) ("1"+acc)
    
    Cov binCov ""
    

let bizFuzz x =
 let rec count a b c d =
    match a>x || x<0 with 
    |true -> (b,c,d)
    |false -> match (a%3=0 && a%5=0) with 
            |true -> count (1+a) (1+b) (1+c) (1+d)
            |false -> match a%3=0 with
                        |true -> count (1+a) (1+b) (c) (d)
                        |false -> match a%5=0 with 
                                |true -> count (1+a) (b) (1+c) (d)
                                |false -> count (1+a) b c d 
 match x<0 with 
  |true -> (0,0,0)
  |false -> count 1 0 0 0

let monthDay days year =
    let _ = 
        match (isLeap year,(days >= 1 && days <= 365),days  >=1 && days <= 366) with
            |true,_,false -> failwith "Invalid number of days entered"
            |false,false,_ -> failwith "Invalid number of days entered"
            |_,_,_-> ()
        
    let rec myMonth counter acc =
        let month,numdays =
            match (counter=2) && ((isLeap year) = true) with
                |true -> "February", 29
                |_ -> month counter
        match acc+numdays >= days with
            |true -> month
            |_ -> myMonth (counter+1) (acc+numdays)
    myMonth 1 0


let coord n =
    let (x1,y1)=n
    let dist (x2,y2)= sqrt ((x1-x2)**2.0 + (y1-y2)**2.0)
    let within (x,y) width height =  match (x1<=(x+width) && x1>=x) && (y1<=y && y1>=y-height) with
    |true -> true
    |_ -> false
    dist,within