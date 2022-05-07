// Question 1.1 
type mymap<'a,'b> = MyMap of list<'a*'b>  

// -- Declare map values dice1 and dice2, representing the two maps (dice1) and (dice2) above.
let dice1 = [(1,4); (2,2); (3,3); (4,2); (5,2); (6,2)]
let dice2 = [(1,4); (2,2); (3,3); (4,3); (5,5); (6,3)]

// -- Explain the type of the two values ex1 and dice1.
// Maps in f# support polymorphic types Map<'a,'b>. In this case the types of the values are mymap and the key is type int and the value mapped to the key is also type int

// Declare an F# function emptyMap() of type unit -> mymap<’a,’b> that returns an empty map.

let emptyMap() = MyMap(List.empty) // function that returns empty mymap 

// Declare an F# function size m of type mymap<’a,’b> -> int that returns the size of the map m. For instance size ex1 returns 3 and size (emptyMap()) returns ().

let size m =
    match m with
    | MyMap [] -> 0
    | MyMap m -> List.length m
 
// Question 1.2
// Declare an F# function isEmpty m of type mymap<’a,’b> -> bool that returns true if the map m is empty; otherwise false. 
// For instance isEmpty ex1 returns false and isEmpty (emptyMap()) returns true.
let isEmpty m = 
    match m with 
    | MyMap [] -> true
    | _ -> false

// Declare an F# function tryFind k m of type ’a -> mymap<’a,’b> -> (’a * ’b) option when ’a : equality
// that returns the value Some (k, v) if k exists in m and v is the value that k is mapped to; otherwise None is returned. 
// For instance tryFind ’B’ ex1 returns Some (’B’, 66) and tryFind ’D’ ex1 returns None.
let tryFind k m = 
    match m with 
    | MyMap [] -> None
    | MyMap m -> List.tryFind (fun (x,_) -> x = k) m

tryFind 2 (MyMap(dice1))

// Declare an F# function remove k m of type ’a -> mymap<’a,’b> -> mymap<’a,’b> when ’a : equality that removes the entry for k in the map m if exists; otherwise m is un- changed. 
// For instance, remove ’B’ ex1 may return the value MyMap [(’A’,65);(’C’,67)].

let remove k m =
    match m with 
    | MyMap m -> 
            let i = List.tryFindIndex (fun (x,_) -> x = k) m // trying to find the index where the key k is 
            match i with
            | None -> MyMap(m) // Returns initial map if index is not there
            | Some(i) -> MyMap(m |> List.removeAt i) // If index is present it returns map with index removed

remove 2 (MyMap(dice1))

// Declare an F# function add k v m of type ’a -> ’b -> mymap<’a,’b> -> mymap<’a,’b> when ’a : equality that returns a new map where the pair (k, v) is added to (or replaced in) the map m depending on whether the key k already exists in m.
// For instance add ’D’ 68ex1mayreturnMyMap[(’D’, 68);(’A’, 65);(’B’, 66);(’C’,67)]andadd ’A’222ex1mayreturnMyMap[(’A’, 222);(’B’,66);(’C’,67)].

let add k v m = 
    match m with 
    | MyMap m -> 
        let i = List.tryFindIndex (fun (x,_) -> x = k) m // trying to find the index where the key k is 
        match i with 
        | None -> MyMap((k,v)::m) // Adds (k,v) to the map if it is not there
        | Some(i) -> MyMap(m |> List.updateAt i (k,v)) // adds (k,v) in the same position as before with new value if it is present. 
           
add 2 10 (MyMap(dice1))

// Question 1.3 
// DeclareanF#functionupdf kvmoftype(’a->’a->’a)->’b->’a->mymap<’b,’a> -> mymap<’b,’a> when ’b : equality that checks whether the key k exists in the map m. If k exists and maps to value v′, then a map m is returned where k is mapped to the combined value with function f, i.e., f v v′.
// If k does not exist, a map m with (k, v) added is returned. For instance upd(+)’A’65ex1mayreturnMyMap[(’A’, 130);(’B’,66);(’C’,67)]andupd (+) ’D’ 68 ex1 may return MyMap [(’D’,68);(’A’,65);(’B’,66);(’C’,67)].

let upd f k v m =
    match m with
    | MyMap m -> 
        let i = List.tryFindIndex (fun (x,_) -> x = k) m // trying to find the index where the key k is 
        match i with 
        | Some(i) -> MyMap(m |> List.updateAt i (k, f v v)) // If index is there 
        | None -> MyMap((k,v)::m) // if index is not there
        
upd (+) 2 2 (MyMap(dice1))

// DeclareanF#functionmapfmoftype(’a->’b->’c)->mymap<’a,’b>->mymap<’a,’c> that returns the map resulting from applying the function f on all entries in the map m
// For in- stancemap(funkv->v+2)ex1mayreturnMyMap[(’A’, 67);(’B’,68);(’C’,69)].

let map f m = 
    match m with
    | MyMap m ->
        MyMap(m |> List.map (fun (k,v) -> (k, f k v)))

map (fun k v->v+2) (MyMap(dice1))

// Declare an F# function fold f s m of type (’a->’b->’c->’a) -> ’a -> mymap <’b,’c> -> ’a that folds the function f over all entries in the map m starting with initial state s
// For in- stance fold (fun s k v -> s+v) 0 dice1 returns 15, i.e., the number of times the dice was rolled.

let fold f s m =
    match m with 
    | MyMap m -> m |> List.fold (fun acc (k,v) -> f acc k v) s // s is the initial state 

fold (fun s k v -> s+v) 0 (MyMap(dice1))

// Question 2.1 

// check if even
let even n = 
    match n with
    | n when n%2 = 0 -> true
    | _ -> false

// make different computations based on whether n is even or not
let collatz n = 
    match n with 
    | n when n%2 = 0 -> n/2
    | _ -> 3*n+1

// throw exception if n<=0
let collatz' n = 
    match n with 
    | n when n%2 = 0 && n>0 -> n/2
    | n when n%2 = 1 -> 3*n+1
    | _ -> failwith "n is zero or less"

collatz 32

// Question 2.2

// works but does not have polymorphic types ...
let applyN f n N = 
    let start = n
    let rec buildList f n N acc =
        let newn = collatz n
        match N with
        | 0 -> start::(List.rev acc)
        | _ -> buildList collatz newn (N-1) ((collatz n)::acc) 
    buildList f n N []
    
applyN collatz 42 8

let applyUntilOne f n =
    let rec findI n i = 
        match n with
        | n when n = 1 -> i
        | _ -> findI (f n) (i+1)
    findI n 0

applyUntilOne collatz 42

// Question 2.3

// consider below and describe the sequence returned by mySeq using mySeq collatz 42 as an example.
// The sequence returned by the recursive function mySeq is the result of the function f turned into a sequence
let rec mySeq f x =
  seq { yield x
        yield! mySeq f (f x)}

mySeq collatz 42 

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


// Adding transactions to a map checking if key is already in map
let addTransToMap ((n,t): trans) m = 
    match m |> Map.tryFind n with
    | None -> m |> Map.add n [t]
    | Some list -> m.Add (n, t::list)

let m1 = addTransToMap ("ISS", ((24,02,2014),100.0,218.99,Buy)) Map.empty 
let m2 = addTransToMap ("ISS", ((22,05,2018),400.0,493.60,Buy)) m1
m2

// Folding the function on the ts tans list 
let shares = List.foldBack addTransToMap ts Map.empty
shares

// Question 3.2
// calculating avg buy price and quantity of shares
let accTrans (tq:float,avg:float) ((d,q,p,tType):transData) =
       match tType with
       | Buy -> (tq+q, ((avg*tq)+(q*p))/(tq+q))
       | Sell ->(tq-q, avg)
    
let quantityAndAvgPrice ts =
    List.fold accTrans (0.0,0.0) ts

quantityAndAvgPrice [((24,02,2014),100.0,218.99,Buy);
                          ((23,02,2016),825.0,280.23,Buy)]

// mapping the quantityAndAvgPrice over the list of shares from 3.1
let res = shares |> Map.map (fun name td -> quantityAndAvgPrice td)  
res

// Question 4.1
// Describe list generated by dup: 
// The function takes each element in the input list and duplicates it by using the cons operator and calling the function recursively
let rec dup = function
    | [] -> []
    | x::xs -> x::x::dup xs 

dup [2;4;6]

// Tail-recursive version of dup
let rec dup1 l acc =
    match l with
    | [] -> List.rev acc
    | x::xs -> dup1 xs (x::x::acc)

dup1 [2;4;6] []

// Question 4.2
// Making a sequence with 2 of variable i using dup1 method
let replicate2 i = seq (dup1 [i] [])

replicate2 4;;

// infinite sequence 
let dupSeq =  Seq.initInfinite (fun i -> dup1 [i] []) |> Seq.concat
    
dupSeq

// Question 4.3 turns a sequence into the same sequence with all elements duplicated
let dupSeq2 s = 
    seq {for i in s do 
         yield i
         yield i
        }

dupSeq2 seq[1;2]