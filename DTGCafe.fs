module DTGCafe

//In order to develop this application, a model for describing all the different categories of drinks as well as their prices need to be developed.

// In this “first sprint” of the mini-project, you will design data type to represent the different types of drinks in the shop/café/ and implement the price calculation function. 

// drink types:  coffee, tea, juice, soda, milk
// drink prices: Small, Medium, and Large


type Size = Small | Medium | Large

type Drinks = 
    | Coffee
    | Tea
    | Juice
    | Soda
    | Milk

type DrinkRecords = {Drink : Drinks; Size: Size; Price: float}
let CoffeeDrink1 = { Drink = Coffee; Size = Small; Price = 1.0}
let CoffeeDrink2 = { Drink = Coffee; Size = Medium; Price = 2.0}
let CoffeeDrink3 = { Drink = Coffee; Size = Large; Price = 3.0}

let TeaDrink1 = { Drink = Tea; Size = Small; Price = 1.0}
let TeaDrink2 = { Drink = Tea; Size = Medium; Price = 2.0}
let TeaDrink3 = { Drink = Tea; Size = Large; Price = 4.0}

let JuiceDrink1 = { Drink = Juice; Size = Small; Price = 1.0}
let JuiceDrink2 = { Drink = Juice; Size = Medium; Price = 5.0}
let JuiceDrink3 = { Drink = Juice; Size = Large; Price = 10.0}

let sizeMultiplyer size =
    match size with
    | Small -> 1.0
    | Medium -> 2.0
    | Large -> 3.0

let drinkMultiplyer drink =
    match drink with
    | Coffee -> 11.0
    | Tea -> 1.0
    | Juice -> 2.0
    | Soda -> 0.5
    | Milk -> 2.5

let price drinkRecord =
    match drinkRecord with
    | { DrinkRecords.Drink = drink; 
        DrinkRecords.Size = size; 
        DrinkRecords.Price = price} -> price * sizeMultiplyer size * drinkMultiplyer drink

price CoffeeDrink1