module _04

type PclShapeUnion = 
    | Rectangles of float * float
    | RightTriangle of float * float
    | Empty

type PclShapeRecord = {name: string; a:float; b:float}

let rect = Rectangles(10.0, 2.0)
let rtri = RightTriangle(10.0, 2.0)

let pclPerimeterUnion shape =
    match shape with
    | Rectangles(a, b) -> 2.0 * ( a + b)
    | RightTriangle(a, b) -> a + b + sqrt(a**2.0 + b**2.0)
    | _ -> 0.0

let pclPerimeterRecord shape =
    match shape with
    | {PclShapeRecord.name = name; PclShapeRecord.a = a; PclShapeRecord.b = b} when name = "rectangle" -> 2.0 * ( a + b)
    | {PclShapeRecord.name = name; PclShapeRecord.a = a; PclShapeRecord.b = b} when name = "right triangle" -> a + b + sqrt(a**2.0 + b**2.0)
    | _ -> 0.0