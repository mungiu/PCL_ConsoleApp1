module _2b

let rec listLength list =
    match list with
    | hd::tl when tl <> [] -> 1 + listLength tl
    | _ -> 1
printfn "%A" (listLength ['x'; 'y'; 'z'])

let rec mySumList lst =
    match lst with
    | [] -> 0
    | hd::rest -> hd + mySumList rest
printfn "%A" (mySumList [2; 3; 5])

//returning a sublist of frst "n" elements
let rec myTakeSome n list = 
        match n with
        | 0 -> []
        | _ -> (List.head list) :: myTakeSome (n - 1) (List.tail list)
printfn "%A" (myTakeSome 2 [2; 3; 5])

///////////// ALTERNATIVES /////////////

let rec pclSum list =
    match list with
    | hd::tl when tl <> [] -> hd + pclSum tl
    | hd::tl when tl = [] -> hd
    | _ -> 0
printfn "%A" (pclSum [2; 3; 5; 8 ])

let rec takeSome qty list =
    match list with
    | hd::tl when qty > 0 -> hd :: takeSome (qty-1) tl
    | hd::tl when qty = 0 -> []
    | _ -> []
printfn "%A" (takeSome 2 ['a'; 'b'; 'c'; 'd'])