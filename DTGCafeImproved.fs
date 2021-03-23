module DTGCafeImproved

type Size = Small | Medium | Large

type Drink = 
    | Coffee
    | Tea
    | Juice
    | Soda
    | Milk

//creating record type
type CoffeeCocktailRecord = {drink:Drink; price:float; size:Size; vatPercentage:float}
type TeaCocktailRecord    = {drink:Drink; price:float; size:Size; vatPercentage:float}
type JuiceCocktailRecord  = {drink:Drink; price:float; size:Size; vatPercentage:float}
type SodaCocktailRecord   = {drink:Drink; price:float; size:Size; vatPercentage:float}
type MilkCocktailRecord   = {drink:Drink; price:float; size:Size; vatPercentage:float}

//creating universal record type
type CocktailRecord = 
    | CCR of CoffeeCocktailRecord
    | TCR of TeaCocktailRecord
    | JCR of JuiceCocktailRecord
    | SCR of SodaCocktailRecord
    | MCR of MilkCocktailRecord
    | None

let sizeMultiplyer size =
    match size with
    | Small -> 1.0
    | Medium -> 2.0
    | Large -> 3.0

let vat amount taxPercentage = 
    amount * (taxPercentage/100.0)

let price cocktail =
    match cocktail with
    | CCR {price = price; size = size; vatPercentage = vatPercentage} 
        -> vat (price * (sizeMultiplyer size)) vatPercentage
    | TCR {price = price; size = size} -> price * sizeMultiplyer size
    | JCR {price = price; size = size} -> price * sizeMultiplyer size
    | SCR {price = price; size = size} -> price * sizeMultiplyer size
    | MCR {price = price; size = size} -> price * sizeMultiplyer size
    | None -> 0.0

let getDrinkTypeString cocktail =
    match cocktail with
    | CCR (coffeeCocktailRecord) -> "coffee"
    | TCR (teaCocktailRecord) -> "tea"
    | JCR (juiceCocktailRecord) -> "juice"
    | SCR (sodaCocktailRecord) -> "soda"
    | MCR (milkCocktailRecord) -> "milk"
    | None -> "nothing"

let printReceipt cocktailRecord quantity =
    match cocktailRecord with
    | CCR (ccr:CoffeeCocktailRecord)
        -> printfn "Please pay: DKK%f for your %d %s with coffee drink(s). Thanks!" 
            (price cocktailRecord)
            quantity 
            (getDrinkTypeString cocktailRecord)
    | TCR (tcr:TeaCocktailRecord)
        -> printfn "Please pay: DKK%f for your %d %s with tea drink(s). Thanks!" 
            (price cocktailRecord)
            quantity 
            (getDrinkTypeString cocktailRecord)
    | JCR (jcr:JuiceCocktailRecord)
        -> printfn "Please pay: DKK%f for your %d %s with juice drink(s). Thanks!" 
            (price cocktailRecord)
            quantity 
            (getDrinkTypeString cocktailRecord)
    | SCR (scr:SodaCocktailRecord)
        -> printfn "Please pay: DKK%f for your %d %s with soda drink(s). Thanks!" 
            (price cocktailRecord)
            quantity 
            (getDrinkTypeString cocktailRecord)
    | MCR (mcr:MilkCocktailRecord)
        -> printfn "Please pay: DKK%f for your %d %s with milk drink(s). Thanks!" 
            (price cocktailRecord)
            quantity 
            (getDrinkTypeString cocktailRecord)
    | None -> printfn "Bye, come again!"

type CoffeeMessage = 
    | OrderDrink of CocktailRecord * int // Drink, qty
    | LeaveAComment of string // ”Comment-super!”

let CoffeeAgent =
    MailboxProcessor.Start( fun inbox ->
        // a function to process the message
        let rec msgLoop = async {
            //read a message
            let! msg = inbox.Receive()
            // process a message
            match msg with
            | OrderDrink (cocktailRecord, quantity) -> printReceipt cocktailRecord quantity
            | LeaveAComment comment -> printfn "The customer comment is: %s" (comment.ToString())
            
            // loop to top
            return! msgLoop
        }
        // startthe loop
        msgLoop)

let message01 = OrderDrink (CCR {drink=Milk; price=2.0; size=Small; vatPercentage=20.0}, 2)
CoffeeAgent.Post(message01);;