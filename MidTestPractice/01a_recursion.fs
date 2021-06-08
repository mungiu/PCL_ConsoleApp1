module _01aExercise

// instantiating variables
let x = 23
let myName = "Bobby"
let age = 25
let country = "Canada"
let y = 6 + 6

let a = 5
// (re)defining the value of a in the equation "a + 5"
let b = 
    let a = 10 
    in a + 5 // b = 10 + 5 = 15
let c = a + b // c = 5 + 15 = 20

let addNum1 number =
    1 + number
printfn "%A" (addNum1 5)
//val addNum1 : number:int -> int
//val it : int = 6

let addNum10 number:int =
    number + 10
printfn "%A" (addNum10 10)
//val addNum10 : number:int -> int
//val it : int = 20

let addNum20 number:int =
    addNum10 (addNum10 number)
printfn "%A" (addNum20 10)
//val addNum20 : number:int -> int
//val it : int = 30

let max2 (first:int) (second:int) =
    if first > second then first
    else second
printfn "%A" (max2 1 10)
//val max2 : first:int -> second:int -> int
//val it : int = 10

let evenOrOdd (integer:int) : string  =
    match integer with
    | int when integer % 2 = 0 -> "even"
    | int when integer % 2 <> 0 -> "odd"
    | _ -> null
printfn "%A" (evenOrOdd 2)
printfn "%A" (evenOrOdd 3)
//> 
//val evenOrOdd : integer:int -> string
//val it : string = "odd"
//> 
//val evenOrOdd : integer:int -> string
//val it : string = "even"

let evenOrOddSimple integer =
    if integer % 2 = 0 then "even" else "odd"
printfn "%A" (evenOrOddSimple 2)
printfn "%A" (evenOrOddSimple 3)
//> 
//val evenOrOddSimple : integer:int -> string
//val it : string = "even"
//> 
//val evenOrOddSimple : integer:int -> string
//val it : string = "odd"

// NOTE: %A is used to print object values that are passed in
let addXY (x:int) (y:int) =
    printfn "%A %A" x y
    x + y
printfn "%A" (addXY 5 3)
//> 
//5 3
//8
//val addXY : x:int -> y:int -> int
//val it : int = 8


// NOTE: People often mistakenly associate recursive with functional and iterative with imperative. 
// A purely functional program cannot be iterative because the value of the condition of a loop never varies. 
// By contrast, an imperative program may be recursive.
let addNumOriginal j k =
    j + 10 * k

let addNum10x number:int =
    number + 10

let rec addNum10Rec j k =
    match j with
    | 0 -> k
    | _ -> addNum10Rec (j-1) (addNum10x k)
printfn "%A" (addNum10Rec 3 5)