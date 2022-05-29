// ** Question 1
type OrderedList<'a when 'a : equality> =
    {front: 'a list;
     rear: 'a list}

let ex = {front = ['x']; rear = ['z';'y']}

exception OrderedListError of string

// Question 1.1
let ol1 = {front = ["Hans"; "Brian"; "Gudrun"]; rear = []}

let ol2 = {front = ["Hans"; "Brian"]; rear = ["Gudrun"]}
let ol3 = {front = ["Hans"]; rear = ["Gudrun";"Brian";]}

// With the three elements above there exist 4 representations of the orderedList

// Question 1.2 
let canonical ol = 
    match ol with
    // Return same orderedList if rear is already empty  
    | {front = x; rear = []} -> ol
    // Reverse rear list and traverse list adding all elements from rear to front list
    | {front = x; rear = y} -> {front = y |> List.rev |> List.fold (fun acc elm -> acc@[elm]) x; rear = []}

canonical ol2 // { front = ["Hans"; "Brian"; "Gudrun"] rear = [] }        
canonical ol1 // { front = ["Hans"; "Brian"; "Gudrun"] rear = [] } 
canonical ol3 // { front = ["Hans"; "Brian"; "Gudrun"] rear = [] }

let toList ol = 
    let can = canonical ol // Make canonical version of OrderedList 
    // Then return front list 
    match can with
    | {front = x; rear = y} -> x

toList ex // [’x’;’y’;’z’]

// Question 1.3 
// Empty orderedList
let newOL() = {front = []; rear = []}

let isEmpty ol = if ol = newOL() then true else false

isEmpty (newOL())
isEmpty ex

// Question 1.4 
let addFront elm ol = 
    match ol with
    | {front = x; rear = y} -> {front = elm::x; rear = y}

addFront 'w' ex // { front = ['w'; 'x'] rear = ['z'; 'y'] }
                           
let removeFront ol = 
    match ol with 
    // If front is empty then we make canonical of OrderedList to fill front list
    | {front = []; rear = y} -> 
        let can = canonical ol 
        // Then we remove front element of list
        match can with 
        | {front = x; rear = y} -> (x |> List.head, {front = x |> List.tail; rear = y})
    | {front = x; rear = y} -> (x |> List.head, {front = x |> List.tail; rear = y})       

removeFront ex // ('x', { front = [] rear = ['z'; 'y'] })

let peekFront ol = 
    match ol with 
    // If front is empty then we make canonical of OrderedList to fill front list
    | {front = []; rear = y} -> 
        let can = canonical ol 
        // Then we return first element of front list
        match can with 
        | {front = x; rear = y} -> x |> List.head
    | {front = x; rear = y} -> x |> List.head 

peekFront ex // 'x'

// Question 1.5
// Gets canonical representation of both orderedLists and returns the orderedLists with the two front lists of the canonical representations put together
let append ol1 ol2 =
    let canOl1 = canonical ol1
    match canOl1 with 
    | {front = x1; rear = y1} -> 
        let canOl2 = canonical ol2 
        match canOl2 with 
        | {front = x2; rear = y2} -> {front = x1@x2; rear = y1}

append ex ex 

// Question 1.6
let map f ol = {front= List.map f ol.front; rear= List.map f ol.rear}
                               
map (fun e -> e.ToString()) ex

// Question 1.7
let fold f s ol = 
    let olList = toList ol
    olList |> List.fold (fun acc e -> f acc e) s

fold (fun acc e -> acc + e.ToString()) "" ex // "xyz"

// Question 1.8
let multiplicity ol = 
    // Getting canonical representation of ol, so we can map front list only
    let canOl = canonical ol
    // Traverse the frontlist recursively adding element to map and finally returning map which is the accummulator 
    let rec multiplicity' ol acc =
        match ol with 
        | {front = []; rear = y} -> acc
        | {front = x::xs; rear = y} -> 
            // Checks if map already contains key - if key is there we increase the value connected to key by 1
            match acc |> Map.tryFind x with 
            | None ->  multiplicity' ({front = xs; rear = y}) (acc |> Map.add x 1)
            | Some e -> multiplicity' ({front = xs; rear = y}) (acc |> Map.add x (e+1))
    multiplicity' canOl Map.empty

multiplicity (addFront 'x' ex)

// ** Question 2
let rec f i = function
      [] -> [i]
    | x::xs -> i+x :: f (i+1) xs

f 10 [0;1;2;3] // [10; 12; 14; 16; 14]

// Question 2.1 

// q1
// f takes an int and a list of integers as input and computes a list of integers that has length = input list' length+1. 
// At each recursive call to f an integer is added to the returned list which is equal to i+x, then i is increased by 1
// and the first element is removed from the input list - continues till input list is empty then i is added in base step

// q2
// Calling f can never return an empty list as the integer i is always added to the returned list. 
// For example: f 10 [] return [10]

// q3
// Calling f can never go into an infinite loop as the length of the returned list will always be input list' length+1, by calling recursively on the tail of the list

// Question 2.2
// Tail-recursive version
let rec fA i lst acc = 
    match lst with 
    | [] -> i::acc |> List.rev
    | x::xs -> fA (i+1) xs ((i+x)::acc)

fA 10 [0;1;2;3] []

// Question 2.3
// Continuation version
let rec fC i lst c = 
    match lst with 
    | [] -> c [i]
    | x::xs -> fC (i+1) xs (fun res -> c((i+x)::res))

fC 10 [0;1;2;3] id

// ** Question 3
let myFinSeq n M = Seq.map (fun m -> n+n*m) [0..M]

// Question 3.1
myFinSeq 10 5 |> Seq.toList
// The sequence returned by myFinSeq is a sequence with M+1 integers.
// The elements in the sequence are: seq [n+n*0, ... ,n+n*M]


// Question 3.2
let mySeq n = Seq.initInfinite (fun i -> n+n*i)

mySeq 10

// Question 3.3
let multTable N M = 
    let Mseq = seq { for i in [0..M] do yield i }
    let Nseq = seq { for i in [0..N] do yield i }
    let pairSeq = Seq.allPairs Mseq Nseq // making all pairs 
    pairSeq |> Seq.map (fun (x,y) -> (x,y,(x*y))) 
  
multTable 10 10 |> Seq.toList  

// Question 3.4
let ppMultTable N M = 
    multTable 10 10 |> Seq.map (fun (x,y,z) -> sprintf "%i * %i is %i" x y z)

Seq.take 4 (ppMultTable 10 10) // seq ["0 * 0 is 0"; "0 * 1 is 0"; "0 * 2 is 0"; "0 * 3 is 0"]

// ** Question 4

