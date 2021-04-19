module Session4

open System.IO

//////////     THE BAD WAY TO DO IT     /////////////
let sizeOfFolder folder =
    let filesInFolder : string [] = Directory.GetFiles(folder, "*.*", SearchOption.AllDirectories)  // get all files under the path
    let fileInfos : FileInfo [] = Array.map(fun (file:string) -> new FileInfo(file)) filesInFolder  // map files to their corresponding FileInfo object
    let fileSize : int64 [] = Array.map(fun(info:FileInfo) -> info.Length) fileInfos                // map fileInfo objects to the file's size
    let totalSize = Array.sum fileSize                                                              // sum the file size
    totalSize                                                                                       // return the total size of files


// The |>, <|, >>, and << operators serve as a way to clean up F# code.
//  ▪ Avoid them if adding them would only add clutter or confusion. 



//////////    THE GOOD WAY TO DO IT using  |>   ////////////

// |> is an infix polymorphic function which simply applies it’s second argument to it’s first.
let sizeOfFolderPiped folder =
    let getFiles path = Directory.GetFiles(path, "*.*", SearchOption.AllDirectories)
    let totalSize = 
        folder
        |> getFiles
        |> Array.map (fun file -> new FileInfo(file))
        |> Array.map (fun info -> info.Length)
        |> Array.sum
    totalSize
// BACKWARD PIPING <| 
//  ▪ accepts a function on the left and applies it to a value on the right.
//  ▪ it allows you to change precedence (the order in which functions are applied)
//  ▪ arguments are evaluated left-to-right
//  ▪ to call a function and pass the result to another function:
//  ▪ add parentheses around the expression or
//  ▪ use the pipe-backward operator
printfn "The result of sprintf is %s" (sprintf "(%d, %d)" 1 2)
printfn "The result of sprintf is %s" <| sprintf "(%d, %d)" 1 2


//////////    THE GOOD WAY TO DO IT USING  >>   ////////////

// (>>) joins two functions together function on the left is called first AKA:  x |> f |> g = (f >> g) x
let sizeOfFolderComposed (*No Parameters!*) =
    let getFiles folder = Directory.GetFiles(folder, "*.*", SearchOption.AllDirectories)  
        // The result of this expression is a function that takes
        // one parameter, which will be passed to getFiles and piped
        // through the following functions.
    getFiles
    >>Array.map (fun file -> new FileInfo(file))
    >>Array.map (fun info -> info.Length)
    >>Array.sum
sizeOfFolderComposed

//// MORE EXAMPLES
let square x = x * x
let toString (x : int) = x.ToString()
let strLen (x : string) = x.Length
let lenOfSquare = square >> toString >> strLen
square 125
lenOfSquare 125

// BACKWARD COMPOSITION <<
//  ▪ takes two functions and applies the right function first and then the left.
//  ▪ It is useful when you want to express ideas in reverse order.
let square1 x = x * x
let negate x = -x
(square1 >> negate) 10  // results in 100
(square1 << negate) 10   // results in -100

// filtering and creating a list of non empty values from the current list
[ [1]; []; [4;5;6]; [3;4]; []; []; []; [9] ] |> List.filter(not << List.isEmpty)