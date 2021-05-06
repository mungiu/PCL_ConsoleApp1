module Exercise2


(*Note that you don't have to insert ";;" after each declaration in the file: this is only
necessary in the interactive environment, to signal that a line of input is ready to
interpret.*)
let x = 23
let myName = "Bobby"
let age = 25
let country = "Canada"
let y = 6 + 6

let a = 5
let b = let a = 10 in a + 5 // in this case, by using "in" we specify how variables are bounds to the expression that follows
let c = a + b

let addNum1 z = z + 1
let addNum10 z : int = z + 10
let addNum20 z = addNum10(addNum10(z))
let o = addNum20(10)

//function that compares to integer numbers
let max2 (first :int ) (second : int) = 
    if first > second then first else second
let maximum = max2 1 2

//function that check for even or odd and prints out
let evenOrOdd number = 
    if number % 2 = 0 then printfn "Even" else printfn "Odd"
evenOrOdd 2

// prints 2 passed in integers and adds them together after
let printThenAdd (first : int ) ( second : int ) = 
    printfn "%d" first
    printfn "%d" second
    first + second
printThenAdd 1 2 |> ignore
let additionResult = printThenAdd 2 5

let addNum101 z : int = z + 10
let rec addNum_jk (number : int ) ( multiplyer : int ) =
    let updatedResult = addNum101 number
    let newMultiplyer = multiplyer - 1
    if newMultiplyer > 0 then 
        addNum_jk updatedResult newMultiplyer
    else updatedResult

let recursiveAddition10 = addNum_jk 2 3
    