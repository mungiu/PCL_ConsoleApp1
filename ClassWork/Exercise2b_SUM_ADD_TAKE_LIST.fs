module Exercise2b

// List [] ;;
// Every list has a head and a tail if not empty

//[1] --> 1::[]
//summing a list using pattern matching
//reursive processing

//counts passed in list length
let rec myListLength lst =
    match lst with
    | [] -> 0 // if the list is empty
    // :: and [] are called constructors, they construct data structures
    | (hd::rest) -> 1 + myListLength rest //if the list containst head and tail (cnotains at least one element)
myListLength [2; 3; 5]

// summing up all elements in a list
let rec mySumList lst =
    match lst with
    | [] -> 0 // if the list is empty
    // :: and [] are called constructors, they construct data structures
    | (hd::rest) -> hd + mySumList rest //if the list containst head and tail (cnotains at least one element)
mySumList [2; 3; 5]

//returning a sublist of frst "n" elements
let rec takeSome n list = 
        match n with
        | 0 -> []
        | _ -> (List.head list) :: takeSome (n - 1) (List.tail list)
takeSome 2 [2; 3; 5]