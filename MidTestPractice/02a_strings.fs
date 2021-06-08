module _02a_match

let vowelToUpper (char:char) =
    match char with
    | 'a' ->'A'
    | 'e'->'E'
    | 'i'->'I'
    | 'o'->'O'
    | 'u'-> 'U'
    | _ -> char
printfn "%A" (vowelToUpper 'a')
printfn "%A" (vowelToUpper 'x')
//val vowelToUpper : char:string -> string
//"A"val it : unit = ()
//"x"val it : unit = ()


let rec vowelToUpperStrRec (str:string) =
    match str with
    | "" -> ""
    | str -> (vowelToUpper str.[0]).ToString() + vowelToUpperStrRec str.[1..(String.length(str) - 1)]
printfn "%A" (vowelToUpperStrRec "Andrei Mungiu")