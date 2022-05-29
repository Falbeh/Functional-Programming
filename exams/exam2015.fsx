// ** Question 1
type multimap<'a,'b when 'a: comparison> =
    MMap of Map<'a,list<'b>>

let ex = MMap (Map.ofList [("record",[50]);("ordering",[36;46;70])])

exception MapError of string

// Question 1.1
let studReg = MMap (Map.ofList [("Grete",[]); ("Hans",["TOPS"; "HOPS"]); ("Peter",["IFFY"]); ("Sine",["HOPS";"IFFY";"BFNP"]) ])

// Yes, that is possible, for studReg = studReg2 to be true they have to be identical.
// If we change the order of the course registrations for a person they will not be equal even though the person is registered for the same courses
// Example where the order of Hans' courses has been changed 
let studReg2 = MMap (Map.ofList [("Grete",[]); ("Hans",["HOPS"; "TOPS"]); ("Peter",["IFFY"]); ("Sine",["HOPS";"IFFY";"BFNP"]) ]) 
studReg = studReg2 // false

// Question 1.2
let canonical m =
    match m with
    | MMap map -> 
        let mapList = map |> Map.toList // Turning map into list so we can use List.map
        let sortedLists = mapList |> List.map (fun (key,lst) -> (key,(lst |> List.sort))) // Mapping over each element in list and sorting the lists with course registrations
        MMap (Map.ofList (sortedLists)) // Returning the multimap with sorted course lists

canonical studReg // MMap (map [])

let toOrderedList m = 
    let orderM = canonical m // Ordering multimap
    // Returning list of multimap
    match orderM with
    | MMap map -> 
        map |> Map.toList

toOrderedList studReg 

// Question 1.3
let newMultimap = MMap (Map.empty)

newMultimap

let sizeMultimap m = 
    match m with 
    | MMap map ->
        let mapList = map |> Map.toList
        let keyCount = mapList.Length // Getting number of keys in list 
        // Traversing each value list and adding number of elements to accummulater - in basestep return the pair (keyCount,valCount)
        let rec sizeMultimap' mapList acc = 
            match mapList with
            | [] -> (keyCount,acc)
            | (key,lst)::xs -> let valueCount = lst |> List.fold (fun acc x -> acc+1) 0 // Could also just add list.Length to acc instead of folding
                               sizeMultimap' xs (acc+valueCount)
        sizeMultimap' mapList 0

sizeMultimap studReg

// Question 1.4
let addMultimap k v m = 
    match m with 
    | MMap map -> 
        match map |> Map.tryFind k with 
        // If key is not there add the key k with value v
        | None -> MMap (map |> Map.add k [v]) 
        // If key is in map, check if the value list contains v, if it does we do nothing (to prevent duplicates), else we add the value to value list
        | Some lst -> if lst |> List.contains v then m else MMap (map |> Map.add k (v::lst)) 

sizeMultimap (addMultimap "Sine" "BFNP" studReg) // (4,6)
sizeMultimap (addMultimap "Sine" "TIPS" studReg) // (4,7)
sizeMultimap (addMultimap "Grete" "TIPS" studReg) // (4,7)
sizeMultimap (addMultimap "Pia" "" studReg) // (5,7)

let removeMultimap k vOpt m = 
    match m with
    | MMap map -> 
        match vOpt with 
        | None -> MMap (map |> Map.remove k) // Remove key with all its values if vOpt is none
        | Some elm -> 
            match map |> Map.tryFind k with
            | None -> m // If key is not in map don't change anything
            | Some lst -> let listWithoutElm = List.filter (fun x -> x <> elm) lst // If key is in map, remove the value from the key
                          MMap (map |> Map.add k listWithoutElm) 

sizeMultimap (removeMultimap "Sine" None studReg) // (3,3)
sizeMultimap (removeMultimap "Sine" (Some "PLUR") studReg) // (4,6)
sizeMultimap (removeMultimap "Kenneth" (Some "BLOB") studReg) // (4,6)
sizeMultimap (removeMultimap "Peter" (Some "IFFY") studReg) // (4,5)

// Question 1.5 
let mapMultimap f m = 
    match m with 
    // Map over the map in multimap - for each key and list pair map over the list and apply function f on each element
    | MMap map -> MMap (map |> Map.map (fun key list -> list |> List.map (fun elm -> f key elm)))

mapMultimap (fun k v -> v+"-F2015") studReg

// Question 1.6 
let foldMultimap f s m = 
    match m with
    // folds over the map in multimap - for each key and list pair fold over list and apply function f on each elm - initial state for Map.fold is s and initial state for List.fold is acc
    | MMap map -> map |> Map.fold (fun acc key list -> list |> List.fold (fun acc' elm -> f acc' key elm) acc) s 
 
foldMultimap (fun acc k v -> String.length v + acc) 0 studReg // 24

// ** Question 2
let rec f i j xs =
    if xs = [] then
        [i*j] 
    else
        let (x::xs') = xs
        x*i :: f (i*j) (-1*j) xs'

f 10 1 [1 .. 9]

// Question 2.1

// q1
// f computes a list of integers same length as the length of xs. 
// Each integer in xs*i is added to a list recursively, and every time f is called i is multiplied with j and j is chaning between a positive and negative integer
// Therefore f computes a list of integers changing between positive and negative

// q2
// The incomplete pattern match issue arises in the else statement.
// When destructuring the statement let (x::xs) = xs, the compiler can't tell if xs at this point is empty.
// Preceeding this statement, multiple other cons statements may have been used. Thus, the case of an empty list is unhandled.
// This is why it is better to handle this using pattern matching, through a match expression.

// In the below method the warning disappers because the matching is happening in the outer pattern match where all cases are covered
let rec fMatch i j xs =
    match xs with 
    | [] -> [i*j]
    | x::xs' -> x*i :: fMatch (i*j) (-1*j) xs'

fMatch 10 1 [1 .. 9]

// Question 2.2 
let rec fMatchA i j xs acc = 
    match xs with 
    | [] -> i*j::acc |> List.rev
    | x::xs' -> fMatchA (i*j) (-1*j) (xs') (x*i::acc)

fMatchA 10 1 [1 .. 9] []

// ** Question 3

let myFinSeq n m = seq { for i in [n .. m] do
                             yield [n .. i] }

// Question 3.1
myFinSeq 2 5 // seq [[2]; [2; 3]; [2; 3; 4]; [2; 3; 4; 5]]
// myFinSeq returns a sequence of lists. 
// The method loops from n to m and at each loop it yields a list to the sequence containing elements from n to i. 

// In myFinSeq 10 14 the value 12 occurs 3 times. The method returns a sequence with 5 lists, whereas 3 of them will have 12 in it.

// Question 3.2
let myFinSeq2 n m = myFinSeq n m |> Seq.collect id

myFinSeq2 3 6 // seq [3; 3; 4; 3; 4; 5; 3; 4; 5; 6]

// Question 3.3
let sum xs = List.fold (fun r x -> r+x) 0 xs
let seq4000 = myFinSeq 10 4000
let array4000 = Array.ofSeq seq4000

// array4000 contains 3991 lists

let sums = Array.map sum array4000 
sums

// Parallel computation of lists: 
let sumsParallel = Array.Parallel.map sum array4000 
sumsParallel


// ** Question 4
