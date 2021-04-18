module _01aExercisePCL1

let x = 23
let myName = "Bobby"
let age = 25
let country = "Canada"
let y = 6 + 6

let a = 5
let b = let a = 10 in a + 5
let c = a + b

let addNum1 number =
    number + 1

let addNum10 (number:int) =
    number + 10

let addNum20 (number:int) =
    addNum10 (addNum10 number)

let max2 first second =
    if first > second then  first
    else second

let evenOrOdd number =
    match number with
    | number when number % 2 = 0 -> "even number"
    | number when number % 2 <> 0 -> "odd number"

let addXY first second =
    printfn "%d" first
    printfn "%d" second
    first + second

let rec addNumRec accumulator multiplier =
    match multiplier with
    | 0 -> accumulator
    | _ -> addNumRec (addNum10 accumulator) (multiplier - 1)

addNumRec 3 5