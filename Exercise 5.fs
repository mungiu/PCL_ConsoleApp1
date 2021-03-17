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



///////// ALTERNATIVE SOLUTION 
// The benefits here is that shapes are first defined and then 
// a record of these shapes is created.
// This allows much more flexibility for the shape records
type RectangleR = {width : float; length: float}
type RightTriangleR = {tbase : float; length : float}
type EquilateralTriangle = {side : float}
type GenericTriangle = {tbase : float; side2 : float; side3 : float; height: float}
type ShapeRecord = 
    | RR of RectangleR 
    | RTR of RightTriangleR 
    | EQTR of EquilateralTriangle 
    | GT of GenericTriangle

let pclAreaRecord shape =
    match shape with
    | RR {width = width; length = length} -> length * width
    | RTR {tbase = tbase; length = length} -> 0.5 * tbase * length
    | EQTR {side = side} -> ((sqrt 3.0)/4.0)*(side**2.0)
    | GT {tbase = tbase; height = height} -> tbase * height / 2.0