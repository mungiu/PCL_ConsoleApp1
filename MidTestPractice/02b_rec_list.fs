module _2b

let rec pmLength list =
    match list with
    | hd::tl when tl <> [] -> 1 + pmLength tl
    | _ -> 1
pmLength ['x'; 'y'; 'z' ]

let rec pclSum list =
    match list with
    | hd::tl when tl <> [] -> hd + pclSum tl
    | hd::tl when tl = [] -> hd
pclSum [2; 3; 5; 8 ]

let rec takeSome qty list =
    match list with
    | hd::tl when qty > 0 -> hd :: takeSome (qty-1) tl
    | hd::tl when qty = 0 -> []
takeSome 2 ['a'; 'b'; 'c'; 'd'] 