// Question 1.1

type mymap<'a,'b> = MyMap of list<'a*'b> 

let ex1 = MyMap [('A',65);('B',66);('C',67)]
let ex1' = MyMap [('C',67);('A',65);('B',66)]

// Declaring map values representing the maps dice1 and dice2

let dice1 = MyMap [(1,4);(2,2);(3,3);(4,2);(5,2);(6,2)]
let dice2 = MyMap [(1,4);(2,2);(3,3);(4,3);(5,5);(6,3)]

// The types of ex1 is mymap with char as key and int as value
// The types of dice1 is also mymap, but here the key is type int and value is also type int
//
let emptyMap () = MyMap []

//
let size m = 
   match m with 
   | MyMap [] -> 0
   | MyMap m -> List.length m 
size ex1 
size (emptyMap())

//
let isEmpty m = if size m = 0 then true else false

isEmpty ex1 // false
isEmpty (emptyMap()) // true

//
let tryFind k m =
    match m with
    | MyMap [] -> None
    | MyMap m -> m |> List.tryFind (fun (x,_) -> x=k)

tryFind 'A' ex1

//
let remove k m = 
    match m with
    | MyMap m -> 
        let i = m |> List.tryFindIndex (fun (x,_) -> x=k) // finding index where k is
        match i with
        | None -> m
        | Some (i) -> m |> List.removeAt i

remove 'B' ex1

//
let add k v m = 
    match m with
    | MyMap m -> 
        let i = m |> List.tryFindIndex (fun (x,_) -> x=k) // finding index where k is
        match i with
        | None -> MyMap((k,v)::m)
        | Some (i) -> MyMap(m |> List.updateAt i (k,v))

add 'D' 68 ex1
add 'A' 222 ex1

// 
let upd f k v m =
    match m with
    | MyMap m -> 
        let i = m |> List.tryFindIndex (fun (x,_) -> x=k) // finding index where k is
        match i with
        | None -> MyMap((k,v)::m)
        | Some (i) -> MyMap(m |> List.updateAt i (k, f v v))


upd (+) 'A' 65 ex1

// 
let map f m = 
    match m with 
    | MyMap m -> MyMap(m |> List.map (fun (k,v) -> (k, f k v)))

map (fun k v -> v+2) ex1 

// 
let fold f s m = 
    match m with 
    | MyMap m -> m |> List.fold (fun acc (k,v) -> f acc k v) s // Doing a List.fold and making my own acc and running over tuples (k,v) on each tuple I then call the defined method f. s being the initial state therefore assigned to acc.

fold (fun s k v -> s+v) 0 dice1 

// Question 2.1
let even n = if n % 2 = 0 then true else false
even 42

//
let collatz n = 
    match n with 
    | n when even n -> n/2
    | _  -> (3*n)+1

collatz 64 // 32

// 
let collatz' n = 
    match n with 
    | n when even n = true && n>0 -> n/2
    | n when even n = false && n>0 -> (3*n)+1
    | _ -> failwith "n is zero or less" 

collatz' 45 // 136
collatz' 0 // Exception

// Question 2.2

let applyN f n N =
    let s = n
    let rec loop n N acc = 
        match N with 
        | 0 -> s::(List.rev acc)
        | _ -> loop (f n) (N-1) (f n::acc)
    loop n N []
    
applyN collatz 42 8 // int list = [42; 21; 64; 32; 16; 8; 4; 2; 1]

//
let applyUntilOne f n = 
    let rec loop n acc =
        match n with 
        | n when n = 1-> acc 
        | _ -> loop (f n) (acc+1)
    loop n 0

applyUntilOne collatz 42 // 8

// Question 2.3 
let rec mySeq f x =
  seq { yield x
        yield! mySeq f (f x)}

// Describing mySeq f x with mySeq collatz 42 as example
// mySeq is building a sequence with the result of the function f - 
// by first adding x to the sequence and then calling mySeq recursively with the yield! where x is now the result of (f x)

let g x = x*2

mySeq g 1 

// Question 3.1

type name = string
type quantity = float
type date = int * int * int
type price = float
type transType = Buy | Sell
type transData = date * quantity * price * transType
type trans = name * transData
let ts : trans list =
  [("ISS", ((24,02,2014),100.0,218.99,Buy));  ("Lego",((16,03,2015),250.0,206.72,Buy));
   ("ISS", ((23,02,2016),825.0,280.23,Buy));  ("Lego",((08,03,2016),370.0,280.23,Buy));
   ("ISS", ((24,02,2017),906.0,379.46,Buy));  ("Lego",((09,11,2017), 80.0,360.81,Sell));
   ("ISS", ((09,11,2017),146.0,360.81,Sell)); ("Lego",((14,11,2017),140.0,376.55,Sell));
   ("Lego",((20,02,2018),800.0,402.99,Buy));  ("Lego",((02,05,2018),222.0,451.80,Sell));
   ("ISS", ((22,05,2018),400.0,493.60,Buy));  ("ISS", ((19,09,2018),550.0,564.00,Buy));
   ("Lego",((27,03,2019),325.0,625.00,Sell)); ("ISS", ((25,11,2019),200.0,680.50,Sell));
   ("Lego",((18,02,2020),300.0,720.00,Sell))]

// 
let addTransToMap (n,td) m =
    match Map.tryFind n m with
    | None -> m |> Map.add n [td]
    | Some list -> m.Add (n, td::list)

let m1 = addTransToMap ("ISS", ((24,02,2014),100.0,218.99,Buy)) Map.empty
let m2 = addTransToMap ("ISS", ((22,05,2018),400.0,493.60,Buy)) m1
m2

// 
let shares = List.foldBack addTransToMap ts Map.empty
shares

// Question 3.2
//
let accTrans (tq:float,avg:float) ((d,q,p,tType):transData) =
       match tType with
        | Buy -> ((tq+q),(((avg*tq)+(q*p))/(tq+q)))
        | Sell -> (tq-q, avg)

let quantityAndAvgPrice ts =
       List.fold accTrans (0.0,0.0) ts

quantityAndAvgPrice [((24,02,2014),100.0,218.99,Buy);
                    ((23,02,2016),825.0,280.23,Buy)]

//
let res = Map.map (fun name td -> quantityAndAvgPrice td) shares 
res


// Question 4.1

// 
let rec dup = function
    | [] -> []
    | x::xs -> x::x::dup xs 

dup [1;2;3;4;5]

// dup duplicates all values in the input list

// Tail-recursive dup
let rec dupA l acc = 
    match l with 
    | [] -> List.rev acc
    | x::xs -> dupA xs (x::x::acc)

dupA [1;2;3;4;5] []

// Question 4.2
let replicate2 i = seq ([i;i])

replicate2 4

let dupSeq = Seq.initInfinite (fun i -> dupA [i] []) |> Seq.concat
dupSeq

// Question 4.3
let dupSeq2 s = 
    seq {for i in s do 
            yield i
            yield i}

dupSeq2 (seq[1;2])