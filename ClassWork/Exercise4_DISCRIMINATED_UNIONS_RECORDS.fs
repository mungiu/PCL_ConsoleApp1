module Exercise4

//type PclRightTriangle = float * float
//type PclRectangle = float * float 
//let pclShape = PclRightTriangle (1.0, 1.0)
//let pclShape' = PclRectangle (0.5, 1.0, 1.0)

// DISCRIMINATED UNIONS - like enums on steroids
type PclShape = 
    | RightTriangle of float * float 
        | Rectangle of float * float

let rectangle = Rectangle(0.5, 0.5)
let rightTriangle = RightTriangle(2.0, 5.0)

let area shape =
    match shape with
    | RightTriangle (triangleHeight, triangleBase) -> 0.5 * triangleHeight * triangleBase
    | Rectangle (length, width) -> length * width

let perimeter shape = 
    match shape with
    | Rectangle (length, width) -> 2.0 * (length + width)
    | RightTriangle (length, width) -> length + width + sqrt(length**2.0 + width**2.0)

area rectangle
area rightTriangle
perimeter rectangle


// convert PclShape to use records
// RECORDS - containers for a set of name:value pairs
type Shapes = RightTriangle | Rectangle
type ShapeRecords = {length: float; width: float; shape: Shapes}

// MINI PROJECT - Drink To Go Online Cafe
// decide which drinks you want to handle
// decide on names of coffee you want to have