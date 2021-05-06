module _01bExercise


let a = 1 :: [2; 5; 3; 7]
printf "%A" a
// val it : int list = [1; 2; 5; 3; 7]

let b = List.length [2; 5; 3; 7];;
printf "%A" b
//val it : int = 4

//// NOTE: Only a list can be added at the end of a list
//let c = [2; 5; 3; 7] :: 4;;

let rec addToEnd list number =
    match list with
    | head :: tail when tail <> [] -> head :: addToEnd tail number
    | head :: tail when tail = [] -> head :: number
    | _ -> list
printf "%A" (addToEnd [2; 5; 3; 7] [4; 8])
printf "%A" (addToEnd [2; 5; 3; 7] [4])
//val it : int list = [2; 5; 3; 7; 4; 8]
//val it : int list = [2; 5; 3; 7; 4]