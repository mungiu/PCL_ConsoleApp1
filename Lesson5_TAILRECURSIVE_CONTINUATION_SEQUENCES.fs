module Lesson_5

let rec sumList lst =
    match lst with
    [] -> 0
    | hd :: tl -> hd + sumList tl
sumList [1 .. 1000000]    // this generates a stack overflow

let rec i_am_tail_recursive arg = 
    // for completeness check for bound
    printf "%d" arg // here arg will have a new value inside each recursive call
    if (arg = 1000) then true
    else i_am_tail_recursive (arg + 1)
i_am_tail_recursive(1000)

// recursive version without StackOverflow
// the last thing done in a function must the the recursive call itself
// nothing else must be done after that to avoid StackOverflow
let sumListRefactored lst =
    let rec sumListAcumTR lst accumulator =
        match lst with
        [] -> accumulator
        | hd :: tl -> sumListAcumTR tl (accumulator + hd)
    sumListAcumTR lst 0
sumListRefactored [1 .. 1000000]

// CONINUATION - simply a function that you pass into another function to tell it what to do next.
// prints a list of integers in reverse using continuation
let printInReverse lst =
    let rec printInReverseTR lst cont =
        match lst with
        // If there is nothing left, execute the continuation function
        [] -> cont()
        // for other lists, add printing of the current
        // element as part of the continuation
        | hd :: tl -> printInReverseTR tl (fun() -> printf "%d" hd 
                                                    cont())
    printInReverseTR lst (fun() -> printfn "DONE")
[0 .. 10] |> printInReverse

//print a list of integers in reverse using the accumulator
let printInReverseRefactored lst =
    let rec printInReverseTR lst accum =
        match lst with
        // executing then continuation function
        [] -> accum
        // for other lists, add printing of the current
        // element as part of the continuation
        | hd :: tl -> printInReverseTR tl (hd :: accum)
    printInReverseTR lst []
[0 .. 10] |> printInReverseRefactored


let rec factorial x =
    if x <= 1 then 1 //base case
    else x * factorial (x-1)

// HERE THE CONCEPT IS OK BUT THE CODE IS WRONG
let accFactorial x =
    let rec tailRecFactorialTR x acc =
        if x <= 1 
        then acc
        else tailRecFactorialTR (x - 1) (acc * x)
    tailRecFactorialTR x 1
accFactorial 10

// IN - Used for sequence expressions and, in verbose syntax, to separate expressions from bindings.
// There are two forms of syntax available for many constructs in the F# language: 
//      verbose syntax - not as commonly used, but has the advantage of being less sensitive to indentation. 
//      lightweight syntax - shorter and uses indentation to signal the beginning and end of constructs, rather than additional keywords like begin, end, in, and so on. 
// The default syntax is the lightweight syntax.
let accFactorialWithContinuation x =
    let rec tailRecFactorialTR x cont =
        if x <= 1 
        then cont 1
        else cont (tailRecFactorialTR (x - 1) (fun(y) -> x * y)) 
    in tailRecFactorialTR x (fun(y) -> y)                       // 'in' keyword specifies where the binding (method  'tailRecFactorialTR') is valid
accFactorialWithContinuation 10

// Using Sequences (sequence elements are generated dinamically whereas lists are stored entirely in memory)
// Sequences help avoid StackOverflow
let seqOfNumbers = seq {1 .. 5}
seqOfNumbers |> Seq.iter (printfn "%d")

//seq<int> = seq [1; 2; 3; 4;]
let a = seq [1; 2; 3; 4;]
let allPositiveIntsList = [for i in 1 .. System.Int32.MaxValue do yield i]      // this will fail with stackOverflow
let allPositiveIntsSeq = seq {for i in 1 .. System.Int32.MaxValue do yield i}   // this will NOT fail with stack overflow

// list of key value pairs
let comingEvents = 
    [
    ("Palmesondag", "march 28");
    ("Skaertorsdag", "april 1");
    ("Langfredag", "april 2");
    ("Paskedag", "april 4");
    ]
    |> Map.ofList
//comingEvents.TryGetValue "Palmesondag"
//comingEvents.Item "Palmesondag"
comingEvents.["Palmesondag"]    // QUICK ACCESS of values from key value pairs

