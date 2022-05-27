// Snippets file for exam in Functional Programming, F2021

// Question 1

type Rope =
  Leaf of string * int
| Node of Rope * int * Rope

let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)),
                 20,
                 Node(Leaf("ogr",3),3,Leaf("amming",6)))

let rope2 = Node(Leaf("_and",4),4,Node(Leaf("_very",6),6,Leaf("much_F#",7)))

// Answer: Rope is monomorphic, as a parent node is always represented as an int, and leaves are always represented as a tuple (string, int)

let rope3 = Node(Node(Leaf("example_",8),8,Leaf("with_",5)),13,Leaf("5_nodes",7))

// ** Question 1.2
let length r =
  let rec length' r acc = 
    match r with 
    | Leaf(_,x) -> acc+x
    | Node(x,_,y) -> length' x acc + length' y acc
  length' r 0

length rope1 // 29

let flatten r = 
  let rec flatten' r acc =
    match r with 
    | Leaf(x,_) -> x+acc
    | Node(x,_,y) -> flatten' x acc + flatten' y acc
  flatten' r ""

flatten rope1 // "I_like_functional_programming"

let maxDepth r = 
  let rec maxDepth' r acc = 
    // I traverse the left subtree and then the right subtree and the depth is returned when I reach a leaf
    match r with
    | Leaf(_) -> acc
    | Node(x,_,y) -> maxDepth' x acc + maxDepth' y acc+1
  maxDepth' r 0

maxDepth rope1 // 3 

let index i r = 
  let s = flatten r // get string
  let c = s.ToCharArray() // explode string into chararray
  let c = c |> List.ofArray // turn array into list
  c |> List.item i
  // s.[i] can also be called instead of exploding string to charArray

index 5 rope1 // 'e'
// test cases
index 28 rope1 // 'g'
index 0 rope1 // 'I'
index 100 rope1 // index out of range
index 2 rope2 // 'n'

// ** Question 1.3

let concat r1 r2 = Node(r1,length r1,r2)
concat rope1 rope2

let rec prettyPrint r = 
  match r with 
  | Leaf(string, value) -> printfn "        Leaf(%s,%i)" string value
  | Node(leftNode, value, rightNode) -> printfn "    Node("
                                        prettyPrint leftNode
                                        printfn "    %i," value
                                        prettyPrint rightNode
                                        printfn ")"
                                                                 
prettyPrint rope1 // I am aware that this is not exactly as the example, but this is what I had time for. 
// Next step would be to count where in the process the pattern-match is so the first "Node(" for example would have no indentation

// Question 2

let list01 = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I';
              'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R';
              'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']

type Bucket<'a> = { sizeBucket : int;
                    elems : List<'a> }

type UList<'a> = Bucket<'a> list

let ulist01 = [ { sizeBucket = 4; elems = ['A';'B';'C';'D'] };
                { sizeBucket = 2; elems = ['E';'F'] };
                { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] };
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

// The type of ulist01 is Bucket<char> list because it makes a list of the Bucket<'a> type where the polymorphic type 'a in elements in the list
// which in this case are defined as chars. 
// The generic type 'a in the type Bucket<'a> is determined by what type of element we put into the list (= List<'a>)
// Further, it is a Bucket<char> list, because we have a list of buckets
let ulist02 = [ { sizeBucket = 5; elems = ['A';'B';'C';'D'; 'E'] };
                { sizeBucket = 1; elems = ['F'] };
                { sizeBucket = 7; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L';'M'] };
                { sizeBucket = 5; elems = ['N'; 'O'; 'P'; 'Q'; 'R'] };
                { sizeBucket = 8; elems = ['S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'] } ]

ulist02 = ulist01 // false because the two Bucket<char> lists do not hold identical lists and the sizebuckets are also not identical

let emptyUL():UList<'a> = [] // come back

// ** Question 2.2
let sizeUL ul = ul |> List.fold (fun acc {sizeBucket = x; elems=_} -> acc+x) 0

sizeUL ulist01 // 26
sizeUL (emptyUL()) // 0

let isEmptyUL ul = if sizeUL ul = 0 then true else false

isEmptyUL (emptyUL()) // true
isEmptyUL ulist01 // false

let rec existsUL e ul =
  match ul with 
  | [{sizeBucket = _; elems = y}] -> 
    match y |> List.tryFind (fun elm -> elm=e) with
      | None -> false
      | Some(e) -> true
  | {sizeBucket = _; elems = y}::xss -> 
      match y |> List.tryFind (fun elm -> elm=e) with
        | None -> existsUL e xss
        | Some(e) -> true 
  | _ -> false

existsUL 'A' (emptyUL()) // false
existsUL 'A' ulist01 // true
existsUL '!' ulist02 // false

let itemUL ul i = 
  let rec itemUL' ul i acc =
    match ul with 
    | [] -> acc |> List.item i // Find item at index i in accummulated list
    | {sizeBucket = _; elems = y}::xss -> itemUL' xss i (y@acc) // adding all lists together into an accummulated list
  itemUL' ul i []
  
itemUL ulist01 5 // 'T'

let filterUL p ul =  // Skipped the rest of question 2 because of lack of time
  let rec filterUL' p ul acc = 
    match ul with 
    | [] -> let tmp = acc |> List.filter (fun {sizeBucket = x; elems = y} -> x>0) // Removing empty sizeBuckets
            tmp |> List.rev // Reversing list 
    | {sizeBucket = _; elems = y}::xss -> let filter = y |> List.filter p
                                          filterUL' p xss ({sizeBucket = filter.Length; elems = filter}::acc)
  filterUL' p ul (emptyUL())

filterUL(fun e -> e < 'I') ulist01 

let ulist03Wrong = [ { sizeBucket = 2; elems = [1] };
                     { sizeBucket = 0; elems = [] };
                     { sizeBucket = 5; elems = [2;3;4;5;6] } ]


// ** Question 2.3 
// Helper method to check for empty buckets and that all bucket sizes are smaller than 5
let rec noEmptyBucket ul = 
    match ul with 
    | [] -> true
    | {sizeBucket = x; elems = y}::xss -> if x = 0 || x>4 then false else noEmptyBucket xss

// Helper method to check if bucket sizes are correct
let rec correctBucketSize ul = 
  match ul with 
  | [] -> true
  | {sizeBucket = x; elems = y}::xss -> if x = y.Length then noEmptyBucket xss else false

let chkUL ul = if noEmptyBucket ul && correctBucketSize ul then true else false

chkUL ulist03Wrong // false 
chkUL ulist01 // false

let map f ul = 
  let rec map' f ul newUL =
    match ul with 
    // Traverse through each list in each Bucket<'a> and apply function f to the elements of the lists. Then add them to new Bucket<'a> list.
    | [] -> newUL |> List.rev // reversing list in base step
    | {sizeBucket = x; elems = y}::xss -> map' f xss ({sizeBucket = x; elems = y |> List.map (fun x -> f x)}::newUL) 
  map' f ul (emptyUL())                                                     
  
map (int) ulist01

let fold f a ul =
  let rec fold' f ul acc = 
    match ul with 
      | [] -> acc 
      | {sizeBucket = x; elems = y}::xss -> fold' f xss (y |> List.fold (fun acc elm -> f acc elm) acc) // Folding over lists in buckets and applying function f to each elm
  fold' f ul a

fold (fun a c -> a+((string)c)) "" ulist01

// Question 3

let G(m,n) =
  let rec G' (m,n) acc =
    match n with 
    | n when n <= 0 -> m+n+acc
    | n when n > 0 -> G' ((2*m),(n-1)) (acc+m)
  G' (m,n) 0

G(10,10)
G(1,1)
G(1,2)

// The function G is tail-recursive as the only thing that is being called in the pattern-match is the function itself and then returning result in base-step
// Old values are not being kept in the stackframe

// ** Question 3.2


let mySeq = let s = Seq.init 100 (fun v -> (v+1,1)) // Makes sequence with 100 elements from [(1,1);(2,1),...,[100,1]
            // Then make new sequence using above sequence to make correct sequence.
            seq {for (x,y) in s do 
                  for i in 1 .. 100 do yield (x,i)
            }

Seq.toList mySeq // used to check result - Tested with 50 instead of 100 to see if the list went correctly from 1,50 to 2,1 
Seq.take 4 mySeq

let gSeq = mySeq |> Seq.map (fun (x,y) -> G (x,y))

Seq.take 4 gSeq

// Question 4

type stack = int list
type inst =
  ADD    
| SUB
| PUSH of int
| LABEL of string
| IFNZGOTO of string
| EXIT

let insts01 =
  [PUSH 10;
   PUSH 12;
   ADD;
   EXIT]

let insts02 =
  [PUSH 10;
   LABEL "sub1";
   PUSH 1;
   SUB;
   IFNZGOTO "sub1";
   EXIT]

let execInsts insts =
  let rec exec insts s =
    match (insts,s) with
      | (EXIT::is,v1::s) -> v1
      | (SUB::is,v1::v2::s) -> exec is (v2-v1::s)
      | (ADD::is,v1::v2::s) -> exec is (v1+v2::s)
      | (PUSH(x)::is,s) -> exec is (x::s)
      | (LABEL lab::_,s) -> failwith "LABEL not implemented"
      | (IFNZGOTO lab::_,s) -> failwith "IFNZGOTO not implemented"
      | _ -> failwith "Missing stack values for instruction"
  exec insts []
               
execInsts insts01

execInsts insts02


// ** Question 4.2

type resolvedInst =
    RADD
  | RSUB
  | RPUSH of int
  | RIFNZGOTO of int
  | REXIT
type prog = Map<int,resolvedInst>

type env = Map<string,int>

let lookup l m =
  match Map.tryFind l m with
    None -> failwith "Value not in map"
  | Some v -> v

// Changed the template a bit, as I like the explicit style og pattern matching more 
let buildEnv insts =
  let rec build insts index env =
    match insts with
    | [] -> env
    | LABEL(lab)::insts -> build insts index (env |> Map.add lab (index)) // If Label occurs then it is added to the map with index as key
    | _ :: insts -> build insts (index+1) env
  build insts 0 Map.empty 
  
buildEnv insts01
buildEnv insts02

let resolveInsts insts env =
  let rec resolve idx = function
    [] -> Map.empty
  | LABEL lab :: insts -> resolve idx insts
  | ADD :: insts -> Map.add idx RADD (resolve (idx+1) insts)
  | IFNZGOTO lab :: insts -> Map.add idx (RIFNZGOTO (lookup lab env)) (resolve (idx+1) insts)
  | SUB :: insts -> Map.add idx RSUB (resolve (idx+1) insts)
  | PUSH(x) :: insts -> Map.add idx (RPUSH x) (resolve (idx+1) insts)
  | EXIT :: insts -> Map.add idx REXIT (resolve (idx+1) insts)
  resolve 0 insts

resolveInsts insts02 (buildEnv insts02) // map [(0, RPUSH 10); (1, RPUSH 1); (2, RSUB); (3, RIFNZGOTO 1); (4, REXIT)]

