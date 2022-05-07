// Question 1
type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>

let psEx = PrioritySet ["a";"b";"c"]

// ** Question 1.1
// ** Consider the following elements and assume they are inserted in an empty priority set in this order: "a", "q", "a", "b", "b", "q", "d", "a".
// ** Declare a value priSetEx, being the result of inserting the elements above according to the definition of a priority set.
let priSetEx = PrioritySet ["a";"q";"b";"d"]

// ** What is the type of the value priSetEx?
// The type of the value of priSetEx is string.

// ** Declare a value empty representing an empty priority set, i.e., priority set with no elements.
let empty = PrioritySet []

// ** Question 1.2
// ** Declare a function isEmpty : PrioritySet<’a> -> bool when ’a : equality that returns true if a priority set is the empty set. For instance isEmpty(empty) returns true. The value empty is defined above.
let isEmpty ps =
    match ps with
    | PrioritySet [] -> true // true if list is empty 
    | PrioritySet (list) -> false // false if PrioritySet contains some list of values

isEmpty(empty)
isEmpty(psEx)

// ** The size of a priority set is the number of elements in the set. Declare a function size : PrioritySet<’a> -> int when ’a : equality that returns the size of a priority set. For instance, size psEx returns 3.
let size psEx = 
    // declaring an inner recursive method to count number of elements in PrioritySet using accumulator 
    let rec loop psEx acc = 
        match psEx with
        | PrioritySet [] -> acc
        | PrioritySet (head::tail) -> loop (PrioritySet(tail)) (acc+1)
    loop psEx 0 // acc starts at 0

size psEx

// ** Declare a function contains e ps of type contains : ’a -> PrioritySet<’a> -> bool when ’a : equality that returns true if the priority set ps contains an element e. For instance contains "b" psEx returns true
let contains e ps = 
    // Similar to size method, but this time checking if the element in first position is e otherwise continue looking until list is empty.
    let rec loop e ps = 
        match ps with 
        | PrioritySet [] -> false
        | PrioritySet (head::tail) -> if head = e then true else loop e (PrioritySet(tail))
    loop e ps

contains "b" psEx

// ** Declare a function getPN e ps of type getPN : ’a -> PrioritySet<’a> -> int when ’a : equality that returns the priority number of element e if exists in priority set ps. Otherwise raises an error exception (failwith). For instance getPN "a" psEx returns 1.
let getPN e ps = 
    // Similar to contains method, but this time returning the position of element e using an accumulator and throwing exception if element is not present
    let rec loop e ps acc = 
        match ps with 
        | PrioritySet [] -> failwith "The given element is not in the PrioritySet"
        | PrioritySet (head::tail) -> if head = e then acc else loop e (PrioritySet(tail)) (acc+1)
    loop e ps 1 // starts at position 1 in PrioritySet


getPN "a" psEx // 1
getPN "q" psEx // exception thrown

// ** Question 1.3
// ** Declare a function remove e ps of type remove : ’a -> PrioritySet<’a> -> PrioritySet<’a> when ’a : equality that removes element e from the priority set ps and returns a new priority set. Nothing changes if e does not exists in ps. For instance, remove "b" psEx returns the priority set PrioritySet ["a";"c"].
let remove e ps = 
    match ps with 
    | PrioritySet (list) -> 
        let i = list |> List.tryFindIndex (fun elm -> elm=e) // finding index where e is
        match i with 
        | None -> ps // returning same PrioritySet if e is not there
        | Some(i) -> PrioritySet (list |> List.removeAt i) // if e is there remove return PrioritySet with e removed

remove "b" psEx // returns the priority set PrioritySet ["a";"c"].

// ** Declare a function add : ’a -> PrioritySet<’a> -> PrioritySet<’a> when ’a : equality where add e ps returns the priority set ps with the element e added with lowest priority (highest priority number) unless already in the set ps. Adding element h to priority set {a1, b2, c3} gives the priority set {a1, b2, c3, h4}. Adding element b to {a1, b2, c3} gives the unchanged priority set {a1, b2, c3}.
let add e ps = 
    match ps with 
    | PrioritySet (list) -> 
        let i = list |> List.tryFindIndex (fun elm -> elm=e) // finding index where e is
        match i with 
        | None -> PrioritySet (list @ [e]) // Adding e if it is not already in PrioritySet
        | Some(i) -> PrioritySet list // if e is there don't do anything 

add "q" psEx // PrioritySet ["a"; "b"; "c"; "q"]
add "b" psEx // PrioritySet ["a"; "b"; "c"]

// ** Question 1.4

// ** Declare a function map f ps of type map : (’a -> ’b) -> PrioritySet<’a> -> PrioritySet<’b> when ’a : equality and ’b : equality
// ** where map f ps returns the priority set where the function f has been applied on all elements in the priority set ps in order of priority number. For instance map (fun (c:string) -> c.ToUpper()) psEx returns the priority set value PrioritySet ["A";"B";"C"].
let map f ps = 
    match ps with 
    | PrioritySet (list) -> PrioritySet (list |> List.map (fun elm -> f elm)) // maps over each element of list in PrioritySet and applies function f

map (fun (c:string) -> c.ToUpper()) psEx // PrioritySet ["A";"B";"C"]

// ** Declare a function cp of type cp : PrioritySet<’a> -> PrioritySet<’b> -> PrioritySet<’a * ’b> when ’a : equality and ’b : equality
// ** where cp ps1 ps2 returns the cartesian product of ps1 and ps2. The result set is generated from ps1 and ps2 in order of priority number of ps1 first and then ps2. For instance the cartesian product of {A1, B2, C3} and {h1, i2} is {(A, h)1,(A, i)2,(B, h)3,(B, i)4,(C, h)5,(C, i)6}. A cartesian product involving an empty set is the empty set, eg. cp psEx empty is the empty set.
let cp ps1 ps2 = 
    match ps1, ps2 with
    | PrioritySet [],  PrioritySet (list) -> empty // if first or second list is empty return empty list
    | PrioritySet (list), PrioritySet [] -> empty
    | PrioritySet (list1), PrioritySet (list2) -> PrioritySet ((list1, list2) ||> List.map2 (fun elm1 elm2 -> (elm1,elm2))) // map through both lists and make pairs. 

// Other solution using List.allPairs
let cp1 (PrioritySet ps1) (PrioritySet ps2) =
    PrioritySet (List.allPairs ps1 ps2)

cp psEx psEx // PrioritySet [("a", "a"); ("b", "b"); ("c", "c")]
cp empty psEx // fails for some reason?


