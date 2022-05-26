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

isEmpty(empty) // true
isEmpty(psEx) // false

// ** The size of a priority set is the number of elements in the set. Declare a function size : PrioritySet<’a> -> int when ’a : equality that returns the size of a priority set. For instance, size psEx returns 3.
let size ps = 
    // declaring an inner recursive method to count number of elements in PrioritySet using accumulator 
    let rec loop ps acc = 
        match ps with
        | PrioritySet [] -> acc
        | PrioritySet (head::tail) -> loop (PrioritySet(tail)) (acc+1)
    loop ps 0 // acc starts at 0

size psEx

// different (library) solution
let size1 (PrioritySet set) = set |> List.length

size1 psEx

// ** Declare a function contains e ps of type contains : ’a -> PrioritySet<’a> -> bool when ’a : equality that returns true if the priority set ps contains an element e. For instance contains "b" psEx returns true
let contains e ps = 
    // Similar to size method, but this time checking if the element in first position is e otherwise continue looking until list is empty.
    let rec loop e ps = 
        match ps with 
        | PrioritySet [] -> false
        | PrioritySet (head::tail) -> if head = e then true else loop e (PrioritySet(tail))
    loop e ps

contains "b" psEx

// different (library) solution
let contains1 e (PrioritySet set) = set |> List.contains e

contains1 "b" psEx

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

// ** Question 2
let f curRow =
    let rec f' = function
        | [] -> []
        | [_] -> [1]
        | xs -> let (x1::x2::xs) = xs
                x1 + x2 :: f' (x2::xs)
    (1 :: f' curRow)

// ** Question 2.1
// ** Describe what f computes given the examples below:
// f [1] gives [1; 1] - Second pattern match [_] is hit and since f is called with 1 cons f' it returns [1;1]
// f [1; 1] gives [1; 2; 1] - the two elements 1 and 1 are added together and put into the list which results in [1;2;1]
// f [1; 2; 1] gives [1; 3; 3; 1] 
// f [1; 3; 3; 1] gives [1; 4; 6; 4; 1]

// f computes a list of numbers starting and ending with 1.
// If there are two or more elements in the list, the last pattern match is true and the two first elements are added and put into the list.
// Then f is called recursively leaving out the first element. This continues until 1 element is left and then 1 is added to the end of the list. 

// ** Question 2.2
// ** Compiling the function f and f’ above gives the warning: warning FS0025: Incomplete pattern matches on this expression. 
// ** For example, the value ’[_]’ may indicate a case not covered by the pattern(s). Write a version of f and f’, called fMatch and fMatch’, without this warning. Explain why the warning disappears.

// In the below method x1::x2::xs is defined in the outer pattern matching, which makes the cases complete. 
let fMatch curRow = 
    let rec fMatch' curRow = 
        match curRow with 
        | [] -> []
        | [_] -> [1]
        | x1::x2::xs -> x1+x2 :: fMatch' (x2::xs) 
    (1 :: fMatch' curRow)

// ** Question 2.3
// ** The function f’ is not tail recursive. Write a tail-recursive version, fA’ of f’ (or fMatch’) using an accumulating parameter.

let fAMatch curRow = 
    let rec fAMatch' curRow acc = 
        match curRow with 
        | [] -> []
        | [_] -> 1::acc |> List.rev // Reversing the list might not be necessary as the list is most likely always the same as its reversed version
        | x1::x2::xs -> fAMatch' (x2::xs) (x1+x2::acc)
    (fAMatch' curRow [1])
 
fAMatch [1; 3; 3; 1]


// ** Question 3
let mySeq s1 s2 =
    seq { for e1 in s1 do
          for e2 in s2 do 
          yield! [e1;e2] }

// ** Question 3.1
// ** Describe the sequence returned by mySeq when called with arbitrary sequences s1 and s2.
// mySeq returns a new sequence with elements from s1 and s2 combined using yield! to add an element from each input sequence to the sequence. Cartesian product

mySeq [3] [3;2] // [3; 3; 3; 2]
mySeq [3;2] [3] // [3; 3; 2; 3]

// ** Can you for any arguments to mySeq generate the following value seq [’A’;’D’;’A’;’E’;’A’;’F’;’B’;’D’;’B’;’E’;’B’;’F’]
// Yes you can: 
mySeq ['A';'B'] ['D';'E';'F'] // enters first loop 2 times and the nested loop 3 times, so it creates list of 2*6 elements. 
// Below method can be used to see result fully
let tolist = 
    let s = mySeq ['A';'B'] ['D';'E';'F'] 
    s |> Seq.toList
tolist

// ** Question 3.2
// ** Declare a function mySeq2 s1 s2 of type seq<’a> -> seq<’b> -> seq<’a * ’b> such that the cartesian product of s1 and s2 is returned. For instance mySeq2 [1;2] [’A’;’B’;’C’] gives the result seq [(1,’A’); (1,’B’); (1,’C’); (2,’A’); (2,’B’); (2,’C’)].
let mySeq2 s1 s2 = 
    let l1 = s1 |> Seq.toList
    let l2 = s2 |> Seq.toList
    let combinedList = l2 |> List.allPairs l1
    let combinedSeq = combinedList |> List.toSeq
    combinedSeq

mySeq2 [1;2] ['A';'B';'C'] // seq [(1,’A’); (1,’B’); (1,’C’); (2,’A’); (2,’B’); (2,’C’)]

// ** Question 3.2
// ** Declare a function mySeq3 of type int -> seq<int>, such that mySeq3 n produces the infinite sequence n^2 − n ∗ i for i >= 0. The identifier i is the index of the element in the sequence. Hint: Consider using Seq.initInfinite.
let mySeq3 n = Seq.initInfinite (fun i -> (int) ((float)n**2.0)-(n*i)) // ** is the same as power
mySeq3 2

// ** Question 4
type DataSpec =
    RangeInt of int * int
    | ChoiceString of string list
    | StringSeq of string
    | Pair of DataSpec * DataSpec
    | Repeat of int * DataSpec

let reg1 = [("a1",("cheese",25));
           ("a2",("herring",4));
           ("a3",("soft drink",5))]

let reg =
        Repeat(3,Pair(StringSeq "a",
        Pair(ChoiceString["cheese";"herring";"soft drink"],
        RangeInt(1,100))))

// ** Question 4.1
// ** Declare an F# value pur, of type DataSpec that is a specification for a purchase like below: 
// ** let pur = [(3,"a2"); (1,"a1")]
// ** The first element in each pair is the number of pieces which we choose to be an arbitrary integer between 1 and 10 (RangeInt). The second element of each pair is an article code specified as a sequence of strings (StringSeq). Use the constructors Pair and Repeat to generate two pairs.

let pur = Repeat(2,Pair(RangeInt(1,10),StringSeq "a"))

// ** Question 4.2
// ** Declare a function genValue ds of type genValue : DataSpec -> string such that genValue returns a string representation of the values generated given the specification ds.
// ** Given the randomness built into the data generator, the result of genValue reg could be: "[(a1,(cheese,69));(a2,(herring,94));(a3,(cheese,50))]"
// ** The randomness does not prohibit the same article name to be used several times, e.g., cheese.
// ** Hint: You need a way to generate random numbers to handle RangeInt and ChoiceString. The
// ** function next (i1, i2) below returns a random integer in the interval [i1, . . . , i2[ using the random
// ** generator rand. You also need a way to generate unique numbers for StringSeq. The function
// ** numGen () below returns a new unique number each time it is called. You may also use the template
// ** for genValue below:

let rand = System.Random()

let next(i1,i2) = rand.Next(i1,i2)

let numGen =
    let n = ref 0
    fun () -> n := !n+1; !n


numGen()
let rec genValue ds = 
    match ds with 
    | RangeInt(i1,i2) -> next(i1,i2).ToString()
    | ChoiceString xs -> xs |> List.item (next(0, xs |> List.length)) 
    | StringSeq x -> x + numGen().ToString()
    | Pair (x1,x2) -> "(" + genValue x1 + "," + genValue x2 + ")"
    | Repeat(n,rest) -> "[" + (Seq.init n (fun _ -> genValue rest) |> Seq.toList |> List.fold (fun x acc -> x + ";" + acc) ("")) + "]" // almost 


(* | Repeat(n,rest) -> match n > 1 with
                        | true -> genValue rest + genValue(Repeat(n-1, rest))
                        | false -> genValue rest *)

                    
genValue reg // "[(a1,(cheese,69));(a2,(herring,94));(a3,(cheese,50))]"

// ** Question 4.3
