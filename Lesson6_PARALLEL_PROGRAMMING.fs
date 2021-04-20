module Lesson_6

let stopWatch = new System.Diagnostics.Stopwatch()
let ResetStopWatch() = stopWatch.Reset(); stopWatch.Start()
let showTime() = printfn "It took %d ms" stopWatch.ElapsedMilliseconds

let isPrimeNumber x =
        let mutable i = 2
        let mutable isFactorFound = false 
        while not isFactorFound && i < x do    
            if x % i = 0 then
                isFactorFound <- true
            i <- i + 1
        not isFactorFound

let numbers = [| for i in 10000000..10004000 -> i|]

// NON-OPTIMIZED version
ResetStopWatch()    // start
let primeInfo = 
    numbers
    |> Array.map (fun x -> (x, isPrimeNumber x))
showTime()          // finish

//OPTIMIZED version
ResetStopWatch()    // start
let primeInfoOptimized = 
    numbers
    |> Array.Parallel.map (fun x -> (x, isPrimeNumber x))
showTime()          // finish

// NOTE this will not work in .NET Core 3.0
// ----------------------------------------
// open System.Windows.Forms;;
// let form = Form(Text = "F# GUI Events", Visible = true, TopMost = true;)
// form.MouseDown
// |> EventFilter (fun args - args.x < 50)
// |> Event.map (fun args -> printfn "%d %d" args.X args.Y)