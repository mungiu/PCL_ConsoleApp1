module Exercise3

let vowelPredicate c =
    match c with
    | 'a' -> true
    | 'e' -> true
    | 'i' -> true
    | 'o' -> true
    | 'u' -> true
    | c -> false

let rec countNumOfVowels str init =
    match str with
    | "" -> init
    | _ -> if vowelPredicate str.[0] 
            then countNumOfVowels (str.[1..String.length str-1]) init + 1 
            else countNumOfVowels (str.[1..String.length str-1]) init

countNumOfVowels "Higher-order functions can take and return functions of any order" 0

//primes are only divisible by itself and 1
let isPrime n =
    let rec check i =
        i > n/2 || (n % i <> 0 && check (i + 1))
    check 2

let nums = [ 16; 17; 3; 4; 2; 5; 11; 6; 7; 18; 13; 14; ]

let rec primesUpTo n =
    match n with
    | _ when n < 2 -> []
    | _ -> if isPrime n then 
            n :: primesUpTo (n-1)
            else primesUpTo (n-1)

// |> - Passes the result of the left side to the function on the right side (forward pipe operator).
let listPrime lst = List.filter isPrime lst

//listPrime nums
primesUpTo 10