// Question 1
type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a> 

// Question 1.1

// representation of heap in example 3
let ex3 = HP(1,HP(2,HP(3,EmptyHP,EmptyHP),HP(5,EmptyHP,EmptyHP)),HP(4,EmptyHP,EmptyHP))

// The type of ex3 is monomorphic because the nodes hold int as values.

// Emptyheap
let empty = EmptyHP

// Declaring an exception heapError
exception HeapError of string

// Question 1.2

// Checks if heap is empty or not returning true/false
let isEmpty h = if h = empty then true else false
// testing
isEmpty ex3 // false

// Counting the number of non-empty nodes using recursion (Not tail-recursive)
let size h = 
    let rec loop h acc = 
        match h with 
        | EmptyHP -> acc
        | HP(x,xss,yss) -> loop xss acc + loop yss acc+1
    loop h 0

size ex3

// Returning the minimum value in a non-empty heap, which by definition must be the value in the root
let find h = 
    match h with
    | HP(x,_,_) -> x
    | EmptyHP -> raise (HeapError "can't find minimum on empty heap")

find ex3

// Check if heap fulfills heap property (almost works..)
let chkHeapProperty h = 
    let rec loop h c = 
        match h with 
        | EmptyHP -> true
        | HP(x,y,z) when x >= c -> if loop y x = true then 
                                    (if loop z x = true then true 
                                        else false) else false                          
        | HP(x,y,z) when x < c -> false
        | _ -> failwith "false input"
    loop h 0

chkHeapProperty ex3

// Question 1.2

// applying the function f to all values in the heap. f is applied on the root value first (maybe ;-))
let rec map f h =
    match h with 
    | HP(x,y,z) -> HP(f x,map f y,map f z)
    | EmptyHP -> EmptyHP

map ((+)1) ex3

// Example of function that breaks heap property
let exFalse = map (fun x -> x*(-1)) ex3
exFalse

// Question 2
let random =
         let rnd = System.Random()
         fun () -> rnd.Next(1,10000)

// Question 2.1

// Generating list of n random number between 1 and 10000
// Alternative solution
(* let genRandoms n =
    let rec loop n l =
        match n with 
        | 0 -> l
        | _ -> loop (n-1) (random()::l)
    loop n []

genRandoms 4 *)

let genRandoms n = Array.init n (fun _ -> random())
genRandoms 10

// Generating the same list using parallel 
let genRandomsP n = Array.Parallel.init n (fun _ -> random())
genRandomsP 10

// Question 2.2
let split xs =
    let l = xs |> List.length // finds the length of the list
    let hl = l/2 // finds index of where to split the list into two
    let f, s = xs |> List.splitAt hl // splits list into two lists f and s
    (f,s)

split [1;2;3;4] // Splitting an even list into two seperate lists.
split [1;2;3;4;5] // Splitting an odd list into two seperate lists making the second list the longer one
split [] // Splitting a list of only 1 element leaving the first list empty

// Returns true if list has length of 0 or 1
let indivisible xs = if xs |> List.length <= 1 then true else false

indivisible [] // true

// Returning the sorted mered list of xs and ys (taking for granted that xs and ys are sorted in the input)
let merge (xs, ys) = 
    let rec loop (xs, ys) acc = 
        match xs, ys with
        | [], [] -> List.rev acc // if both lists are empty return accumulated list reversed so it has smallest first
        | x::tx, [] -> loop (tx, ys) (x::acc) // if xs is not empty but ys is
        | [], y::ty -> loop (xs, ty) (y::acc) // if xs is empty but ys is not
        | x::tx, y::ty when x <= y -> loop (tx, ys) (x::acc) // if none of the lists are empty
        | x::tx, y::ty when y < x -> loop (xs, ty) (y::acc) // if none of the lists are empty
    loop (xs, ys) []

merge ([1;2;4;5], [1;2;3;4;5;5])

// Question 2.3
// Using the methods above as helper methods to make the mergesort method
let divideAndConquer split merge indivisible p =
        let rec dc p =
            if indivisible p
                then p
            else
                match (split p) with
                | (l1, l2) -> merge (dc l1, dc l2)
        dc p

divideAndConquer split merge indivisible [22;746;931;975;200]

// Question 3.1
// Secuence with and without cache

let triNum = Seq.initInfinite (fun elm -> (elm*(elm+1))/2)
triNum

let triNumC = Seq.cache triNum
triNumC

// Question 3.2
// function that goes into inifinite loop
let rec filterOddIndex s =
    Seq.append (Seq.singleton (Seq.item 0 s))
        (filterOddIndex (Seq.skip 2 s))
filterOddIndex triNum

// Own version of method that does not loop infinitely using seq.delay to prevent
let rec myFilterOddIndex s = 
    Seq.append (Seq.singleton (Seq.item 0 s))
        (Seq.delay (fun () -> filterOddIndex (Seq.skip 2 s)))
filterOddIndex triNum

// Question 3.3
// Declaring a Seq.zip method using sequence expressions 

let rec zipSeq s1 s2 =
       seq {let e1 = Seq.item 0 s1
            let e2 = Seq.item 0 s2
            (e1,e2)
            yield! (zipSeq (Seq.skip 1 s1) (Seq.skip 1 s2)) // yield! used to generate sequence 
            }

zipSeq triNum triNum

// Question 4.1
exception FigError of string

type Point = P of double * double
type Fig =
    | Circle of Point * double
    | Line of Point * Point
    | Move of double * double * Fig
    | Combine of Fig list
    | Label of string * Fig
    | Ref of string

let figEx01 = Combine [Circle(P(1.0,1.0),2.0);Line(P(0.0,0.0),P(1.0,1.0))]

// Figure representing a square
let rectEx = Combine [Line(P(-1,1),P(1,1)); Line(P(1,1),P(1,-1)); Line(P(1,-1),P(-1,-1)); Line(P(-1,-1),P(-1,1))]

// Returning a square from orthogonal coordinates
let rect (x1,y1) (x2,y2) = Combine [Line(P(x1,y1),P(x2,y1)); Line(P(x2,y1),P(x2,y2)); Line(P(x2,y2),P(x1,y2)); Line(P(x1,-y2),P(x1,y1))]

rect (-2.0,1.0) (1.0,-1.0) 

// Question 4.2

let figEx02 =
    Combine [Label("c",Circle(P(0.0,0.0),1.0));
        Move(1.0,1.0,Ref "c");
        Move(2.0,2.0,Ref "c")] 

// Making a map 
let buildEnv f =
    match f with
    | Label (ref,fig) -> Map [(ref,fig)]
    | Combine (list) -> 
        let rec loop list acc =
            match list with 
            | Label(ref,fig)::xs -> loop xs (Map.add ref fig acc)
            | _::xs -> loop xs acc
            | _ -> acc 
        loop list Map.empty
    | _ -> Map.empty

let envEx02 = buildEnv figEx02

envEx02

// Question 4.3 - This assignment is bad..

(* let substFigRefs env fig =
    let newList = []
    match fig with
    | Label (ref,fig) -> let lbl = env |> Map.tryFind ref
                         match lbl with
                            | Some(x) -> x
                            | None -> None
    | Ref (s) ->         let lbl = env |> Map.tryFind ref
                         match lbl with
                            | Some(x) -> x
                            | None -> None 
    | Combine (list) -> 
        let rec substFigRefs' env list newList =
            match list with
            | Label(ref, fig)::xs -> let lbl = env |> Map.tryFind ref
                                     match lbl with
                                     | Some(x) -> substFigRefs' env xs x::newList 
                                     | None -> None
            
            | Move(d1,d2,fig)::xs ->  *)

    




