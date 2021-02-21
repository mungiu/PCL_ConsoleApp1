module Session1

// one line comment

(*
multi
line
comment
*)

// alt + Enter (evaluate selected code)

// my first F# value
let niceThursday = "A nice thursday"

let num1 = 29;;
let num2 = 29;;
let addScore = num1 + num2;

let f x = 2 * x * x - 5 * x + 3
let funResult = f (addScore + 2)

(*first recursive function
calculating factorial of an integer*)

// factorial formula n! = n * (n-1)!, n > 0
// rec - used for telling F# that this is a recursive function
let rec factorial1 x = 
    if x < 1 then 1 
    else x * factorial1 (x - 1)
let num3 = factorial1(10)

//tuple - a structure where we can mix different types (like one row in a data table)
let tdata = (11, "February", -5.50)

//list of numbers
let oddNums = [1; 3; 5; 7]


1 :: [2; 5; 3; 7];;         // adding list of integers to integer
List.length [2; 5; 3; 7];;  // creating integer with value of list length
[2; 5; 3; 7] @ [4];;        // adding a list of numbers to another list fo numbers