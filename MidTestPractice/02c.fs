module _02c

let rec pclFold myFunc accumulator list =
    match list with
    | [] -> accumulator
    | hd::tl -> pclFold myFunc (myFunc accumulator hd) tl
printfn "%A" (pclFold (+) 0 [1; 2; 3])
//6
//val pclFold :
//  myFunc:(int -> 'a -> int) -> accumulator:int -> list:'a list -> int
//val it : unit = ()

let pclSumWithFold list =
    match list with
    | hd::tl when tl <> [] -> pclFold (+) hd tl
    | hd::tl when tl = [] -> hd
    | _ -> 0
printfn "%A" (pclSumWithFold [2; 3; 5; 8 ])
//18
//val pclSumWithFold : list:int list -> int
//val it : unit = ()

let rec pclFoldBack myFunc accumulator list =
    match list with
    | hd::tl when tl <> [] -> myFunc hd (pclFoldBack myFunc accumulator tl)
    | hd::tl when tl = [] -> hd
    | _ -> accumulator
printfn "%A" (pclFoldBack (+) 0 [1; 2; 3])
//6
//val pclFoldBack :
//  myFunc:('a -> 'a -> 'a) -> accumulator:'a -> list:'a list -> 'a
//    when 'a : equality
//val it : unit = ()

let pclSumWithFoldBack list =
    match list with
    | hd::tl when tl <> [] -> pclFoldBack (+) hd tl
    | hd::tl when tl = [] -> hd
    | _ -> 0
printfn "%A" (pclSumWithFoldBack [2; 3; 5; 8])

let rec pclIncList list =
    match list with
    | hd::tl when tl <> [] -> (hd+1)::(pclIncList tl)
    | hd::tl when tl = [] -> [hd+1]
    | _ -> []
printfn "%A" (pclIncList [2; 3; 1; 4])

let addNum1 number =
    1 + number

let pclMap myFunc list =
    match list with
    | hd::tl when tl <> [] -> (myFunc hd) :: (pclIncList tl)
    | hd::tl when tl = [] -> [myFunc hd]
    | _ -> []
printfn "%A" (pclMap (addNum1) [2; 3; 1; 4 ])

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