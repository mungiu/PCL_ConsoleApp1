module Session3

//Type inference, inferese type from the expression
let rec incList (ls:double list) =
    match ls with
    |[]->[]
    |head::tail -> head + 1.0 :: incList tail

// takes a function that returns a function that takes another function that returns a list
// if "function" keywod would not be used, these inferences would not work
let rec pclMap f = function 
    |[] -> []
    |lsthd::lsttail -> f lsthd :: pclMap f lsttail

//defining a function      defining a function within a function           using the function defined inside
let incList2 ls =          let inc n = n + 1                               in pclMap inc ls

let negate n = -n;
pclMap negate [1..10]
//val it : int list = [-1 -2 -3 -4 -5 -6 -7 -8 -9 -10]
//pcFilter : ('a -> bool) -> `a list - > ` a list

//the predicate
let isEven n = 
    if n % 2 = 0 
    then true 
    else false

//the higher order function "filter" that can use a passed in predicate
let rec pclFilter predicate lst =
    match lst with
    | [] -> []
    | head::tail -> if predicate  head then head::pclFilter predicate tail
                    else pclFilter predicate tail
pclFilter isEven [0;1;2;3;4;5]



////////////////// FOLD VARIANTS

//sum of all numbers [1;2;3]
//looks are the list
let rec pclFold f init lst =
    match lst with
    | [] -> init
    | head::tail -> pclFold f (f init head) tail

//passing in a lambda funtion that adds two numbers, the initial favlue 0
pclFold (fun x y -> x + y) 0 [1;2;3]  //returns the sum of the list


let closureFun x =
    let multi x y = x * y
    let triple = multi 3
    printfn "%d" (triple 5)

closureFun 5