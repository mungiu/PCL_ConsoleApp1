module TestQuestionAnsweres

let myInfoTuple = (27373, "Andrei", "Mungiu")
let deconstructMyInfo myInfo =
    for element in myInfo do
    printf element

// todo refactor without using "::" - :: IS NOT USED CORRECTLY HERE
let pclAppend ls1 ls2 =
    ls1 :: ls2

//pclAppend [1, 2, 3] [1' 2' 3]

let rec getMax intList prevMax=
    let mutable _max = 0
    match intList with
    | [] -> prevMax
    | hd::tl -> if hd > _max then _max <- hd
                                  getMax tl _max
                else getMax tl _max


