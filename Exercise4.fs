module Exercise4

//type PclRightTrianle = int * int
//type PclRectangle = float * float * 0.5
type PclShape = RightTriangle of float * float | Rectangle of float * float
//let pclShape = PclRightTriangle (1.0, 1.0)
//let pclShape' = PclRectangle (0.5, 1.0, 1.0)

let rectangle = Rectangle(0.5, 0.5)
let rightTriangle = RightTriangle(2.0, 5.0)

let area shape =
    match shape with
    | RightTriangle (triangleHeight, triangleBase) -> 0.5 * triangleHeight * triangleBase
    | Rectangle (length, width) -> length * width

let perimeter shape = 
    match shape with
    | Rectangle (length, width) -> 2.0 * (length + width)

area rectangle
area rightTriangle
perimeter rectangle


// convert PclShape to use records
type PclShapeR = {a: float; b: float}

// MINI PROJECT - Drink To Go Online Cafe
// decide which drinks you want to handle
// decide on names of coffee you want to have