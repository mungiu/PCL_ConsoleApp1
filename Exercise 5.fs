module Exercise_5

// Redefine the pclArea function (defined in Exercise 6.1) to use the new data type (PclShapeR).
// Call the new function pclAreaR

// convert PclShape to use records
type Shapes = RightTriangle | Rectangle
type ShapeRecords = {length: float; width: float; shape: Shapes}
let rectangle = {length = 2.0; width = 2.0; shape = Rectangle}
let rightTriangle = {length = 2.0; width = 2.0; shape = RightTriangle}

let areaR shape = 
    match shape with
    |{  ShapeRecords.length = length;
        ShapeRecords.width = width;
        ShapeRecords.shape = RightTriangle} -> 0.5 * length * width
    |{  ShapeRecords.length = length;
        ShapeRecords.width = width;
        ShapeRecords.shape = Rectangle} -> length * width
rectangle |> areaR

let perimeterR shape = 
    match shape with
    |{  ShapeRecords.length = length;
        ShapeRecords.width = width;
        ShapeRecords.shape = Rectangle}  -> 2.0 * (length + width)
    |{  ShapeRecords.length = length;
        ShapeRecords.width = width;
        ShapeRecords.shape = RightTriangle} -> length + width + sqrt(length**2.0 + width**2.0)