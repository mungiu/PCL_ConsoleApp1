module Session2

let addFloat (x : float) y = x + y

let rec factorial2 (n:int) =
    if n = 0 then
        1
    else
        n * factorial2 (n-1)



let rec factorial3 n = 
    match n with
    | 0 -> 1
    | _ -> if n > 0 then n * factorial3 (n-1) else failwith "YOu have inserted a negative number. Please insert only positive numbers."  // "_" is a wild card that matches anything
factorial3 -1

// Vector operation
//vecAdd : (float * float) -> (float * float)  -> (float * float) 
//vecSub : (float * float) -> (float * float)  -> (float * float)
//vecLen : (float * float) -> float

//let vectorAdd (x1, y1) (x2, y2) = (x1 + x2) * (y1 + y2) : (float * float)