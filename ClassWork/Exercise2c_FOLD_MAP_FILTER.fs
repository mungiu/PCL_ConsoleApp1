module Exercise2c

let rec myFold foldFn accumulator iterable =
    match iterable with
    | [] -> accumulator //if empty return accumulator
    | head::tail -> foldFn head (myFold foldFn 0 tail)
myFold (+) 0 [1; 2; 3] 




let addNum10 z : int = z + 10
// a higher level function that will apply the passed in function to all elements of the passed in list
// and return a new resultant list 
let rec mySumWithFold foldFn accumulator iterable =
    match iterable with
    | [] -> accumulator //if empty return accumulator
    | head::tail -> foldFn head :: mySumWithFold foldFn [] tail
mySumWithFold addNum10 [] [1; 2; 3]



let rec myFoldBack foldFn accumulator iterable =
    match iterable with
    | [] -> accumulator //if empty return accumulator
    | head::tail -> accumulator + foldFn (myFoldBack foldFn 0 tail) head 
myFoldBack (+) 0 [1; 2; 3]



// summing up all elements in a list using myFoldBack
let mySumWithFoldBack lst =
    match lst with
    | [] -> 0 // if the list is empty
    // :: and [] are called constructors, they construct data structures
    | (hd::rest) -> myFoldBack (+) hd rest //if the list containst head and tail (cnotains at least one element)
mySumWithFoldBack [2; 3; 5]

//adding 1 to each element in the list
let rec myIncList lst =
    match lst with
    | [] -> []
    | head::tail -> head + 1 :: myIncList tail
myIncList [1;2;3]

let rec myMap f lst =
    match lst with
    | [] -> []
    | head::tail -> f head :: myMap f tail
myMap addNum10 [1;2;3]

let incWithOne x = x + 1
let rec myIncListWithMap lst =
    match lst with
    | [] -> []
    | _ -> myMap incWithOne lst
myIncListWithMap [1;2;3]

let returnIfEven x =
    if x % 2 = 0 
    then true
    else false

let rec myFilter predicate lst =
    match lst with
    | [] -> lst
    | head::tail -> 
        if predicate head 
        then head :: myFilter predicate tail 
        else myFilter predicate tail
myFilter returnIfEven [1;2;3]