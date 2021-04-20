module Exercise_6a

type IntegerTree = 
    | Node of int * IntegerTree * IntegerTree
    | Empty

// a simple way to sum a tree using recursion
let rec sumIntegerTree tree =
    match tree with
    | Empty -> 0
    | Node (data, left, right) 
        -> data + sumIntegerTree left + sumIntegerTree right

// using tuples
let countWordLetterTuples (str:string) =
    let wordCount = str.Split [|' '|]
    let letterCount = wordCount |> Array.sumBy (fun w -> w.Length)
    (wordCount.Length, letterCount)
countWordLetterTuples "asd asd"
// using records
type tempR = {wordCount: int; letterCount: int}
let countWordLetterRecords (str:string) =
    let wordCount = str.Split [|' '|]
    let letterCount = wordCount |> Array.sumBy (fun w -> w.Length)
    {wordCount = wordCount.Length; letterCount = letterCount}
countWordLetterRecords "dsa dsa"
// Example to be used fo Mini Project - CONCURRENCY is achieved here
// CONCURENCY - important when scalability and large amount of messages are used
// simple asynchronicity can be used, but that is NOT concurrent
// CONCURRENCY is NOT blocking thing whereas asynchronicity blocks things
// looping an returning all messages, then looping and waiting for another one
// working with THREADS is more heavy weight
// remember that messages will be discriminated unions
let PrintAgent =
    MailboxProcessor.Start( fun inbox ->
        // a function to process the message
        let rec msgLoop = async {
            //read a message
            let! msg = inbox.Receive()
            // process a message
            printfn "message is: %s" msg
            // loop to top
            return! msgLoop
        }
        // startthe loop
        msgLoop)

PrintAgent.Post "11222"

// type of agent
type Agent<'T> = MailboxProcessor<'T>
// union type to be sent to agent
type CounterMessage =
    | Count of int
    | Reset

// Message queues in architecture are used to separate parts of applications
// Here we are using our own COnstructor functions
let counterAgent =
    new Agent<_>(fun inbox ->
        let rec loop n = async {
            printfn "n = %d, waiting..." n
            let! msg = inbox.Receive()
            match msg with
            | Reset -> return! loop 0
            | Count value -> return! loop (n + value)
        }
        loop 0)

// example test with many orders for mini project
// orderList |> List.map cafeAgent.Post     // printing the list
// daysOrder |> Chart.Pie |> Chart.Show     // showing the list as a pie chart
// questions might be asked