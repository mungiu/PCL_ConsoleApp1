module Exercise2a

// NOTE
let vowelToUpper c =
    match c with
       | 'a' -> 'A'
       | 'e' -> 'E'
       | 'i' -> 'I'
       | 'o' -> 'O'
       | 'u' -> 'U'
       | c -> c


let rec upperVowelStr str =
    match str with
    | "" -> "" // if empty do nothing
    | str -> (vowelToUpper(str.[0])).ToString() + // if something exists take the first character, convert it to string and call "vowelToUpper" on it
                upperVowelStr(str.[1..String.length str-1]) // perform this entire function on the remaining characters