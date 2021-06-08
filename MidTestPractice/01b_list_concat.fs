module _01bExercise


let a = 1 :: [2; 5; 3; 7]
printf "%A" a
// [1; 2; 5; 3; 7]
// val it : int list = [1; 2; 5; 3; 7]

let b = List.length [2; 5; 3; 7];;
printf "%A" b
//val b : int = 4

//// NOTE: Only a list can be added at the end of a list
//let c = [2; 5; 3; 7] :: 4;;

let rec addToEnd list number_list =
    match list with
    | head :: tail when tail <> [] -> head :: addToEnd tail number_list
    | head :: tail when tail = [] -> head :: number_list
    | _ -> list
printf "%A" (addToEnd [2; 5; 3; 7] [4; 8])
printf "%A" (addToEnd [2; 5; 3; 7] [4])
//val it : int list = [2; 5; 3; 7; 4; 8]
//val it : int list = [2; 5; 3; 7; 4]

// making a list out of a number
let x = 4 :: []
printf "%A" x.Length