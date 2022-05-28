type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>

let ex = MSet (Map.ofList [("a",1);("b",2);("c",1)])

let wrong = MSet (Map.ofList [("a",0);("b",2);("c",1)])

// ** Question 1.1

let diceSet = MSet (Map.ofList [(1,2);(2,1);(3,5);(5,2);(6,2)]) 

// The value of diceSet is Multiset<int> because the generic type 'a (which is the key in the map) is set as int. 

// Answer question 3
let abc x = MSet (Map.ofList [(System.Math.Sin x, 1); (System.Math.Cos x, 1);(System.Math.Sin x, 1)])


// ** Question 1.2
let newMultiset():Multiset<'a> = MSet Map.empty

let isEmpty ms = if ms = newMultiset() then true else false

isEmpty (newMultiset()) // true 
isEmpty diceSet // false

// ** Question 1.3
let add k ms =
    match ms with
    | MSet(map) ->
        match map |> Map.tryFind k with // Looking if element is already in map of multiset
        | None -> MSet (map |> Map.add k 1)
        | Some elm -> MSet (map |> Map.add k (1+elm))

add "a" ex // MSet (map [("a", 2); ("b", 2); ("c", 1)])

let del k ms = 
    match ms with
    | MSet(map) ->
        match map |> Map.tryFind k with // Looking if element is already in map of multiset
        | None -> ms
        | Some elm -> MSet (map |> Map.remove k)

del "c" ex // MSet (map [("a", 1); ("b", 2)])
del "d" ex // Does not remove anything: MSet (map [("a", 1); ("b", 2); ("c", 1)])

// ** Question 1.4

let toList (MSet ms) =
    let msList = Map.toList ms
    // Traverse the map of the multiSet and add keys to a list, then call toList' recursively
    let rec toList' ls acc = 
        match ls with
        | [] -> acc |> List.rev
        | (k,v)::xss -> if v > 1 then toList' ((k,(v-1))::xss) (k::acc) else toList' xss (k::acc) 
    toList' msList []

toList ex

// Using the add method already defined, we can add elements from a list recursively to the Multiset
let fromList xs = 
    let rec fromList' xs acc = 
        match xs with 
        | [] -> acc
        | x::tail -> fromList' tail (add x acc)
    fromList' xs (newMultiset())

fromList ["a";"a";"b"]

// ** Question 1.5

let map f ms = toList ms |> List.map (fun e -> f e) |> fromList

map (fun (c:string) -> c.ToUpper()) ex // MSet (map [("A", 1); ("B", 2); ("C", 1)])

let fold f a ms = toList ms |> List.fold (fun e -> f e) a 

fold (fun acc e -> acc+e) "" ex // abbc

// ** Question 1.6
let union ms1 ms2 = 
    // turning ms2 into a list and then recursively add the elements from the list to ms1
    let ms2List = toList ms2 
    let rec union' ms1 ls = 
        match ls with 
        | [] -> ms1
        | x::tail -> union' (add x ms1) tail
    union' ms1 ms2List  

union ex ex

let minus ms1 ms2 = 
    // Similar to union method but this time removing instead of adding
    let ms2List = toList ms2 
    let rec union' ms1 ls = 
        match ls with 
        | [] -> ms1
        | x::tail -> union' (del x ms1) tail
    union' ms1 ms2List  

minus ex ex 

// ** Question 2.1

let rec f n =
    if n < 10 then "f" + g (n+1) else "f"
  and g n =
    if n < 10 then "g" + f (n+1) else "g"

// All even numbers as arguments to f will produce a string that starts and ends with f
// Also all arguments >=10

f 0 // fgfgfgfgfgf
f 2 // fgfgfgfgf
f 4 // fgfgfgf

// You can only generate the result string: gfgfgfg using the function g, by giving 4 as argument: 
g 4 // gfgfgfg

// You can not start an infinite computation for any argument to f, because if you give a number that is >= 10 then it will evaluate to "f" 
// And for smaller numbers it will call g recursively which calls f recursively, and each time g and f a called the int value is increased by 1

// Tail-recursive version using pattern matching 
let rec fA1 n acc =
    match n with 
    | n when n < 10 -> gA1 (n+1) (acc+"f") 
    | _ -> acc+"f"
  and gA1 n acc =
    match n with  
    | n when n < 10 -> fA1 (n+1) (acc+"g")
    | _ -> acc+"g"

gA1 0 "" // gfgfgfgfgfg

// Another solution using if-then-else
let rec fA n acc =
    if n < 10 then gA (n+1) (acc+"f") else "f"
  and gA n acc =
    if n < 10 then fA (n+1) (acc+"g") else "g"

gA1 0 "" // gfgfgfgfgfg

// ** Question 3.1

let myFinSeq (n,m) = seq { for i in [0 .. n] do
                             yield! seq { for j in [0 .. m] do yield j }}

myFinSeq(1,2) // seq [0; 1; 2; 0; 1; 2]
let seqList = myFinSeq(1,3) |> Seq.toList // [0; 1; 2; 3; 0; 1; 2; 3]

// myFinSeq returns a sequence with integers from 0 to m, n times. 

// It is not possible to generate the sequence [0; 1; 2; 0; 1; 2; 0; 1] because in this case the m argument must be 2, 
// and the outer for loop would yield the sequence [0;1;2] to the final sequence n+1 times. But in this case the sequence does not end with [...0;1;2]


// ** Question 3.2
let myFinSeq2 (n,m) = 
    seq { for i in [0 .. n] do
          yield i, seq { for j in [0 .. m] do yield j }
    }
                             
myFinSeq2 (0,0) // seq [(0, seq [0])]
myFinSeq2 (1,1) // seq [(0, seq [0; 1]); (1, seq [0; 1])]
myFinSeq2 (2,1) // seq [(0, seq [0; 1]); (1, seq [0; 1]); (2, seq [0; 1])]

// ** Question 4

type Row = int
type Col = char
type CellAddr = Row * Col
type ArithOp = Add | Sub | Mul | Div
type RangeOp = Sum | Count
type CellDef =
    FCst of float
    | SCst of string
    | Ref of CellAddr
    | RangeOp of CellAddr * CellAddr * RangeOp
    | ArithOp of CellDef * ArithOp * CellDef
type CellValue =
    S of string
    | F of float
type Sheet = Map<CellAddr,CellDef> 



let header = [((1,'A'),SCst "#EYES");((1,'B'),SCst "1");((1,'C'),SCst "2");
            ((1,'D'),SCst "3");((1,'E'),SCst "4");((1,'F'),SCst "5");
            ((1,'G'),SCst "6");((1,'H'),SCst "Total")]

let result = [((2,'A'),SCst "RESULT");((2,'B'),FCst 2.0);((2,'C'),FCst 1.0);
            ((2,'D'),FCst 5.0);((2,'E'),FCst 0.0);((2,'F'),FCst 2.0);
            ((2,'G'),FCst 2.0);((2,'H'),RangeOp((2,'B'),(2,'G'),Sum))]

let calcPct col = ArithOp(FCst 100.0, Mul, ArithOp(Ref(2,col),Div,Ref(2,'H')))

let pct = [((3,'A'),SCst "PCT");((3,'B'),calcPct 'B');((3,'C'),calcPct 'C');
            ((3,'D'),calcPct 'D');((3,'E'),calcPct 'E');((3,'F'),calcPct 'F');
            ((3,'G'),calcPct 'G');((3,'H'),calcPct 'H')]

let dice = Map.ofList (header @ result @ pct)

dice

// ** Question 4.1 

let heightsHeader = [((4,'B'),SCst "NAME"); ((4,'C'),SCst "HEIGHT")] 
let heightsPeople = [((5,'B'),SCst "Hans"); ((5,'C'),FCst 167.40);
                     ((6,'B'),SCst "Trine"); ((6,'C'),FCst 162.30);
                     ((7,'B'),SCst "Peter"); ((7,'C'),FCst 179.70)]
let heightsCalcs =  [((9,'B'),RangeOp((5,'B'),(7,'B'),Count)); ((9,'C'),ArithOp(RangeOp((5,'C'),(7,'C'),Sum),Div,Ref(9,'B')))]

let heights = Sheet (heightsHeader @ heightsPeople @ heightsCalcs)
heights

// ** Question 4.2 

let getF = function
      F f -> f
    | S s -> failwith "getF: expecting a float but got a string"


let evalRangeOp xs op = 
    match op with 
    | Count -> xs |> List.length |> float // If RangeOp is Count then count elements in list of CellValues
    | Sum -> // If RangeOp is Sum then sum all floats recursively or throw exception if string (this could also be accomplished with List.fold)
        let rec evalSum xs acc =
            match xs with 
            | [] -> acc
            | x::xss -> evalSum xss (acc+(getF x))
        evalSum xs 0.0

evalRangeOp [F 33.0; F 32.0] Sum // 65.0
evalRangeOp [] Sum // 0.0
evalRangeOp [F 23.0; S "Hans"] Sum // throws System.Exception 
evalRangeOp [F 23.0; S "Hans"] Count // 2.0

let evalArithOp v1 v2 op =
    match op with
    | Add -> getF v1 + getF v2
    | Sub -> getF v1 - getF v2
    | Mul -> getF v1 * getF v2
    | Div -> getF v1 / getF v2

evalArithOp (F 33.0) (F 32.0) Sub // 1.0
evalArithOp (S "Hans") (F 1.0) Add // throws System.Exception

// ** Question 4.3

let rec evalValue v sheet =
  match v with
    FCst f -> F f
  | SCst s -> S s
  | Ref ca -> evalCell ca sheet
  | RangeOp ((r1,c1),(r2,c2),op) -> F (evalRangeOp [(evalCell (r1,c1) sheet); (evalCell (r2,c2) sheet)] op) // This should also find values for everything inbetween first cell and second cell, so sum is not working correctly now              
  | ArithOp (v1,op,v2) -> F (evalArithOp (evalValue v1 sheet) (evalValue v2 sheet) op) // Doesn't quite work
and evalCell ca sheet =
  match Map.tryFind ca sheet with
    None -> S "" // We define an empty cell to be the empty string value.
  | Some v -> evalValue v sheet

evalCell (3,'G') dice // the cell value: F 16.67
evalCell (2,'H') dice // 4.0

// (RangeOp((2,'B'),(2,'G'),Sum))
// ** Question 4.4

let ppBoard (sheet: Sheet) = sheet |> Map.fold (fun (acc:string) (x:int,y:char) z -> printfn "%s %c" acc y) ""
    

