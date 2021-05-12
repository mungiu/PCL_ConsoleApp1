module _06

type IntegerTree =
    | Node of int * IntegerTree * IntegerTree
    | Empty

type WordLetterCountRecord = {wordCount:int; letterCount:int}

let intTree = Node(1,
                    Node(2, Empty, Empty),
                    Node(3,
                        Node(2, Empty, Empty),
                        Node(3, Empty, Empty)))

let rec sumIntegerTree integerTree =
    match integerTree with
    | Node (number, left, right) -> number + sumIntegerTree left + sumIntegerTree right
    | _ -> 0
printfn "%A" (sumIntegerTree intTree)

// uses tuples
let countWordnLetterTuple (str:string) =
    let wordCount = str.Split [|' '|]
    let letterCount = wordCount |> Array.sumBy (fun w -> w.Length)
    (wordCount.Length, letterCount) // this is a tuple
    // test it
printfn "Word/Letter counts are: %A" (countWordnLetterTuple "be happy everything is gonna be okay")

// uses records
let countWordnLetterRecord (str:string) =
    let wordCount = str.Split [|' '|]
    let letterCount = wordCount |> Array.sumBy (fun w -> w.Length)
    {wordCount=wordCount.Length; letterCount=letterCount} // this is a record
    // test it
printfn "Word/Letter counts are: %A" (countWordnLetterRecord "be happy everything is gonna be okay")