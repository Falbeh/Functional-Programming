// Snippets file for exam in Functional Programming, F2021

// Question 1

type Rope =
  Leaf of string * int
| Node of Rope * int * Rope

let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)),
                 20,
                 Node(Leaf("ogr",3),3,Leaf("amming",6)))

let rope2 = Node(Leaf("_and",4),4,Node(Leaf("_very",6),6,Leaf("much_F#",7)))

// The type Rope is monomorphic as its types are defined. 

let rope3 = Node(Node(Leaf("example_",8),8,Leaf("with_",5)),13,Leaf("5_nodes",7))

// ** Question 1.2
let length r =
  let rec length' r acc = 
    match r with 
    | Leaf(_,x) -> acc+x
    | Node(x,_,y) -> length' x acc + length' y acc
  length' r 0

length rope1

let flatten r = 
  let rec flatten' r acc =
    match r with 
    | Leaf(x,_) -> x+acc
    | Node(x,_,y) -> flatten' x acc + flatten' y acc
  flatten' r ""

flatten rope1

let maxDepth r = 
  let rec maxDepth' r acc = 
    match r with
    | Leaf(_) -> acc
    | Node(x,_,y) -> maxDepth' x acc + maxDepth' y acc+1
  maxDepth' r 0

maxDepth rope1

let index i r = 
  let s = flatten r // get string
  let c = s.ToCharArray() // explode string into chararray
  let c = c |> List.ofArray // turn array into list
  c |> List.item i
  
index 5 rope1 // 'e'
index 4 rope3 // 'p'
index 10 rope2 // 'u'

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

// The type of ulist01 is Bucket<char> list because it makes a list of the Bucket<'a> type where the polymorphic type 'a  the elements in the list
// which in this case are defined as chars. 

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

let itemUL ul i = 
  let rec itemUL' ul i acc =
    match ul with 
    | [] -> acc |> List.item i
    | {sizeBucket = _; elems = y}::xss -> itemUL' xss i (y@acc)
  itemUL' ul i []
  
itemUL ulist01 5 // 'T'

let filterUL p ul =  // Skipped the rest of question 2 because of lack of time
  match ul with 
  | 

filterUL(fun e -> e < 'I') ulist01 

let ulist03Wrong = [ { sizeBucket = 2; elems = [1] };
                     { sizeBucket = 0; elems = [] };
                     { sizeBucket = 5; elems = [2;3;4;5;6] } ]


// ** Question 2.3 

chkUL ulist03Wrong

chkUL ulist01

map (int) ulist01

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
  | IFNZGOTO lab :: insts -> Map.add idx (RIFNZGOTO (lookup ...)) (resolve ...)
  | ...
  resolve 0 insts

resolveInsts insts02 (buildEnv insts02)
