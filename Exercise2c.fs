module Exercise2c

let rec myFold foldFn accumulator iterable =
    match iterable with
    | [] -> 
        accumulator //if empty return accumulator
    | head::tail ->
        foldFn head (myFold foldFn 0 tail)
myFold (+) 0 [1; 2; 3] 




let addNum10 z : int = z + 10
// a higher level function that will apply the passed in function to all elements of the passed in list
// and return a new resultant list 
let rec mySumFold foldFn accumulator iterable =
    match iterable with
    | [] -> 
        accumulator //if empty return accumulator
    | head::tail ->
        foldFn head :: mySumFold foldFn [] tail
mySumFold addNum10 [] [1; 2; 3]



let rec myFoldBack foldFn accumulator iterable =
    match iterable with
    | [] -> 
        accumulator //if empty return accumulator
    | head::tail ->
        accumulator + foldFn (myFoldBack foldFn 0 tail) head 
myFoldBack (+) 0 [1; 2; 3]



// summing up all elements in a list using myFoldBack
let mySumList lst =
    match lst with
    | [] -> 0 // if the list is empty
    // :: and [] are called constructors, they construct data structures
    | (hd::rest) -> myFoldBack (+) hd rest //if the list containst head and tail (cnotains at least one element)
mySumList [2; 3; 5]