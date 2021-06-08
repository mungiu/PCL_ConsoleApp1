module _02c

// Fold
let rec pclFold func accumulator list =
    match list with
    | [] -> accumulator
    | hd::tl -> pclFold func (func accumulator hd) tl
printfn "%A" (pclFold (+) 0 [1; 2; 3])

let pclSumWithFold list =
    match list with
    | hd::tl when tl <> [] -> pclFold (+) hd tl
    | hd::tl when tl = [] -> hd
    | _ -> 0
printfn "%A" (pclSumWithFold [2; 3; 5; 8 ])


// Fold-back
let rec pclFoldBack myFunc accumulator list =
    match list with
    | hd::tl when tl <> [] -> myFunc hd (pclFoldBack myFunc accumulator tl)
    | hd::tl when tl = [] -> hd
    | _ -> accumulator
printfn "%A" (pclFoldBack (+) 0 [1; 2; 3])

let pclSumWithFoldBack list =
    match list with
    | hd::tl when tl <> [] -> pclFoldBack (+) hd tl
    | hd::tl when tl = [] -> hd
    | _ -> 0
printfn "%A" (pclSumWithFoldBack [2; 3; 5; 8])


// Reduce
let rec pclReduce func list =
    match list with
    | [] -> 0
    | hd::tl -> func hd (pclReduce func tl)
printfn "%A" (pclReduce (+) [1; 2; 3])

let pclSumWithReduce list =
    match list with
    | [] -> 0
    | hd::tl -> pclReduce (+) list
    | _ -> 0
printfn "%A" (pclSumWithReduce [2; 3; 5; 8 ])


// Map
let rec pclMap func list =
    match list with
    | [] -> []
    | head::tail -> func head :: pclMap func tail

let addNum1 number : int =
    number + 1
printfn "%A" (pclMap addNum1 [1; 2; 3])


// Filter
let rec pclFilter predicate list =
    match list with
    | hd::tl when not (predicate hd) -> pclFilter predicate tl
    | hd::tl when predicate hd -> hd :: pclFilter predicate tl
    | _ -> []

let pclEven number =
    match number with
    | number when number % 2 = 0 -> true
    | number when number % 2 <> 0 -> false
    | _ -> false
printfn "%A" (pclFilter (pclEven) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9])