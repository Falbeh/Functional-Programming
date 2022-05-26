// ** Question 1.1

//
let infSeq3 = Seq.initInfinite (fun i -> i*3)
infSeq3

//
let finSeq3 n = infSeq3 |> Seq.take n 
finSeq3 3

// 
let sumSeq3 n = finSeq3 n |> Seq.fold (fun acc i -> acc+i) 0
sumSeq3 100

// ** Question 1.2
let seqMap2 f s1 s2 =
    seq { for (x,y) in Seq.zip s1 s2 do
            yield f x y }

// The function seqMap2 combines the two sequences into a sequence of pairs and then calls the function f on all pairs through a loop.
// An example can be seen below where 1 is added to the x value through function f and therefore evaluates to: seq<int * int> = seq [(2, 1); (3, 2)]
seqMap2 (fun x y -> (x+1,y)) [1; 2] [1; 2] 

// The following calls seqMap2 swap [1;3;3] [4;5;2] does not work, because the function takes seqMap2 yields: f x y and not f (x,y). So the input to the function is not correct.
let swap (x,y) = (y,x) 
// seqMap2 swap [1;3;3] [4;5;2]

// 
let fix = (fun s x y -> s (x,y))
seqMap2 (fix swap) [1;3;3] [4;5;2]


// ** Question 2.1
type TrieNode<'a when 'a : equality> = TN of 'a * bool * TrieNode<'a> list

let trie01 = TN('a',false,[TN('n',false,[TN('d',true,[])])])

//
let trie03 = TN('a',
                false,
                [TN('n',true, [TN('d',true,[])]); 
                 TN('d',false,[TN('d',true,[])]); 
                 TN('t',true,[])
                ])

let trie04 = TN('a',false,[TN('n',true,[TN('d',true,[])]); TN('d',false,[TN('d',true,[])]); TN('t',true,[TN('x',false,[])])])

// The type of trie04 is TrieNode<char>, so the value is char, which is monomorphic. However, the TrieNode type is polymorphic and can other types.

//
exception TrieError of string

// ** Question 2.2

// 
let numLetters t = 
    let rec numLetters' t = 
        match t with 
        | TN(_,_,[]) -> 1
        | TN(_,_, t2) -> 1 + (t2 |> List.fold (fun acc i -> acc + numLetters' i) 0)
    numLetters' t

numLetters trie04
// 
let numWords t = 
    let rec numWords' t = 
        match t with
        | TN(_,false,[]) -> 0
        | TN(_,true,[]) -> 1
        | TN(_,x, t2) ->
            if x = true then 1 + (t2 |> List.fold (fun acc i -> acc + numWords' i) 0)
            else (t2 |> List.fold (fun acc i -> acc + numWords' i) 0)
    numWords' t

numWords trie04

// 
let rec exists ls t = 
    match ls, t with
    | x::[], TN(l,bool,t2) -> if x=l && bool then true else false
    | x::xs, TN(l,bool,t2) when x = l -> t2 |> List.fold (fun s i -> if s then s else exists xs i) false                    
    | x::xs, TN(l,bool,t2) when x <> l -> false
    | _ -> raise (TrieError "wrong input")

exists ['a';'t';'x'] trie04 // false

// 
let rec chkTrie t = 
    match t with 
    | TN (_,false,[]) -> false
    | TN (_,true,[]) -> true
    | TN (_,_,t2) -> t2 |> List.fold (fun s t -> chkTrie t) false 

chkTrie trie04


//
let rec map f t = 
    match t with 
    | TN (v, bool, []) -> TN((f v),bool,[])
    | TN (v, bool, t2) -> TN(f v, bool, t2 |> List.map (fun t -> map f t))

map (string) trie03 //  TN("a", false,[TN ("n", true, [TN ("d", true, [])]);TN ("d", false, [TN ("d", true, [])]); TN ("t", true, [])])
// the type of the result value is string. 


// ** Question 3.1
// where i is the interest rate per annum compounded n times per year from the kth period to the (k + 1)th period.
let rec F m i n k = 
    match k with
    | k when k <= 0 -> m
    | k when k > 0 -> F ((m*(1.0+i/n))) i n (k-1)

F 100.0 0.1 1.0 0 // 100
F 100.0 0.1 1.0 10 // 259.374246 

// The function F is tail-recursive as the only thing that is being called in the pattern-match is the function itself and then returning result in base-step
// Old values are not being kept in the stackframe

// ** Question 3.2
let rec tabulate f start step stop =
    match start with 
    | start when start<step -> [start, f start] @ (tabulate f (start+step) step stop)
    | start when start<stop -> [step, f start] @ (tabulate f (start+step) step stop)
    | start when start>=stop -> [stop, f start] 

tabulate (F 100.0 0.1 1.0) 0 2 4

// 
let prettyPrint xs = 
    let rec prettyPrint' xs acc =
        match xs with
        | (x,y)::[] ->  printfn "  %i | %.2f" x y
        | (x,y)::xs ->  if acc = 0 then
                            printfn "  x | f(x)"
                            printfn "----+------"
                            printfn "  %i | %.2f" x y 
                            prettyPrint' xs (acc+1)
                        else 
                            printfn "  %i | %.2f" x y
                            prettyPrint' xs acc
    prettyPrint' xs 0
                    
prettyPrint [(0, 100.0); (2, 121.0); (4, 146.41)]

// ** Question 4.1

let dt(d,m,y) = System.DateTime(y, m, d)

exception Error of string

type Position =
| Stock of string
| Cash of float

type Action =
| Aquire of System.DateTime * Position
| Give of System.DateTime * Position
| Scale of int * Action
| All of Action list

let ex1 =
  Scale(100,All[Aquire (dt(1,2,2018),Stock "APPLE");
                Give (dt(1,2,2018),Cash 300.3)])

let sellApple = 
    Scale(100,All[Give (dt(1,3,2018),Stock "APPLE");
        Aquire (dt(1,3,2018),Cash 400.4)])
sellApple

let price (s,d) = 
    match s with
    | "APPLE" -> 
        match d with 
        | d when d = (dt(1,2,2018)) -> 300.3
        | d when d = (dt(1,3,2018)) -> 400.4
        | _ -> raise (Error "No price for this date") 
    | "ISS" -> 
        match d with
        | d when d = (dt(1,2,2018)) -> 150.0
        | d when d = (dt(1,3,2018)) -> 200.2
        | _ -> raise (Error "No price for this date") 
    | "TIVOLI" ->
        match d with
        | d when d = (dt(1,2,2018)) -> 212.0
        | d when d = (dt(1,3,2018)) -> 215.2
        | _ -> raise (Error "No price for this date") 


price ("ISS",dt(1,3,2018))

// ** Question 4.2

let buyStock n s d = Scale(n,All[Aquire (d,Stock s); Give(d,Cash (price (s,d)))])

buyStock 100 "APPLE" (dt(1,2,2018))

let receiveCash c d = Aquire(d,Cash c)

receiveCash 100000.0 (dt(1,2,2018))

// ** Question 4.3
let actions =
    let d1 = dt(1,2,2018)
    let d2 = dt(1,3,2018)
    All [receiveCash 100000.0 d1;
         buyStock 100 "APPLE" d1;
         buyStock 200 "ISS" d1;
         buyStock 50 "TIVOLI" d2]

type stockEnv = Map<string,int>

let updStock s n m =
  match Map.tryFind s m with
    None -> Map.add s n m
  | Some n1 -> Map.add s (n+n1) m

type env = float * stockEnv

let emptyEnv = (0.0,Map.empty)

// 
let updEnv scaling (cash,stockEnv) pos = 



updEnv 100 emptyEnv (Cash 100.0)