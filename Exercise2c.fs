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
myFold addNum10 [] [1; 2; 3] 