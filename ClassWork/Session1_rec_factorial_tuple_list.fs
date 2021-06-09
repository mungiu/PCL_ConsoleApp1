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
// Construction
let student1 = (111401, "Gais")
// Triple tuple
let student2 = (111402, "Aron", "IoT")
// Deconstruction – using fst, snd or pattern
let studentId = fst (111401, "Gais")
let studentName = snd (111401, "Gais")
let (x', y') = student1


//list of numbers
let oddNums = [1; 3; 5; 7]

// :: - puts an element in front of a list
let list1 =     1 :: [2; 5; 3; 7];;         // adding and integer in front of a list
let int1:int =  List.length [2; 5; 3; 7];;  // creating integer with value of list length
let list2 =     [2; 5; 3; 7] @ [4] @ [5; 6];;        // adding a list of numbers to another list fo numbers
let int2:int =  List.head [1; 2; 3] // 1
let list3 =     List.tail [1; 2; 3] // [2; 3]
//With optional step value, the result becomes a list 
//of values in the range between two numbers 
//separated by the stepping value
let tens = [0 .. 10 .. 50] : int list //[0; 10; 20; 30; 40; 50]

let numbersNear x = [ 
                        yield (x-1)
                        yield x 
                        yield (x+1)
                    ];;
printfn "%A" (numbersNear 3)

let xList = [ let negate x = -x
        for i in 1 .. 10 do
            if i % 2 = 0 then
                yield negate i
            else
                yield i 
        ];;

// Generate the first ten multiples of a number 
let firstTenMultiplesOf x = [ for i in 1 .. 10 do yield x * i ]
printfn "%A" (firstTenMultiplesOf 2)
// Simplified list comprehension 
let firstTenMultiplesOff x = [ for i in 1 .. 10 -> x * i ]
printfn "%A" (firstTenMultiplesOff 2)

// for - allows use of patterns
let pclSqrs n = [ for i in 1 .. n -> (i, i*i) ]
let pclSqrsAdd n = [ for (i, psq) in pclSqrs n -> i + psq ]
printfn "%A" (pclSqrsAdd 10)


// yield!
let list4 = [
                for a in 1 .. 5 do
                match a with
                | 3 -> yield! ["p"; "c"; "l"] // if current value is 3 add the sublist ["p"; "c"; "l"]
                | _ -> yield a.ToString() // adding current value to the list as a sublist of string
            ]


// yield
let listWithYield = [ 
                        for i in 0 .. 10 .. 20 do
                        yield [ i .. 1 .. i+9 ]
                    ]

let has3 = List.exists (fun x -> x=3) xList
List.rev xList // reverse list
let has3x = List.tryFind (fun x -> x=3) xList
List.zip [1; 2] [3; 4] // [(1, 3); (2, 4)]
let filtered3s = List.filter (fun x -> x=3) xList
List.partition (fun x -> x=3) xList // ([3], [1; -2; -4; 5; -6; 7; -8; 9; -10])
