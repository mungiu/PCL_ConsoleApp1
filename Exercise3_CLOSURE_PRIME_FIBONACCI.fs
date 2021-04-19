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



//PRIMES are only divisible by itself and 1
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



//EX 3.3 FIBONACCI - the next number is the sum of the previous two
let rec recFibonacci first second count =
    let currentCount = count - 1;
    let currentNumber = first + second
    match currentCount with
    | 0 -> if second = 0 then 0 else second         // if count 0
    | _ -> match currentNumber with                       // if count not 0
            | 0 -> recFibonacci first 1 currentCount   // if first number is 0
            | _ -> recFibonacci second currentNumber currentCount
let myFibonaci n = recFibonacci 0 0 n
myFibonaci 9



//EX 3.4
let doubleNum x = x * 2
let sqrNum x = x * x
let pclQuad x = doubleNum (doubleNum x)
let pclFourth x = sqrNum (sqrNum x)