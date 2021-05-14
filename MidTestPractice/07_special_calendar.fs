module _07a

// ERA DAY - means starting with year ONE "1"

let isLeapYear year = (year % 400) = 0 || ((year % 4) = 0 && (year % 100) <> 0)
printfn "This should be true: %A" (isLeapYear 2000)
printfn "This should be true: %A" (isLeapYear 2020)
printfn "This should be false: %A" (isLeapYear 2019)
printfn "This should be false: %A" (isLeapYear 2021)

let leapYearsEraDayToYear year =
    [for i in 1 .. year -> i] 
    |> List.filter isLeapYear 
    |> List.length
printfn "This should be 485: %A" (leapYearsEraDayToYear 2000)

let totalEraDaysToEndOfYear year =
    365 * year + (1 * leapYearsEraDayToYear year)
printfn "This should be 365: %A" (totalEraDaysToEndOfYear 1)
printfn "This should be 654515: %A" (totalEraDaysToEndOfYear 1792)

// With a correction to allow for the possibility that y may be a leap-year. -0/-1/-2
let totalEraDaysToEndOfMonthYear month year =
    match month with
    | month when month = 0              -> (367 * month + 5) / 12 - 0 + totalEraDaysToEndOfYear (year - 1)
    | month when month >= 1 && month < 13 
        ->  match year with 
            | year when isLeapYear year         -> (367 * month + 5) / 12 - 1 + totalEraDaysToEndOfYear (year - 1)
            | year when not (isLeapYear year)   -> (367 * month + 5) / 12 - 2 + totalEraDaysToEndOfYear (year - 1)
            | _ -> 0
    | _ -> 0
printfn "This should be 654423: %A" (totalEraDaysToEndOfMonthYear 9 1792)
printfn "This should be 227027: %A" (totalEraDaysToEndOfMonthYear 7 622)

let totalEraDaysToDateMonthYear date month year =
    date + (totalEraDaysToEndOfMonthYear (month-1) year)
printfn "This should be 1: %A" (totalEraDaysToDateMonthYear 1 1 1)
printfn "This should be 227015: %A" (totalEraDaysToDateMonthYear 19 7 622)
printfn "This should be 654415: %A" (totalEraDaysToDateMonthYear 22 9 1792)
printfn "This should be 729825: %A" (totalEraDaysToDateMonthYear 12 3 1999)





//////////////////// given the eraDay calculate the year in which it falls
// 1 - Each 400-year period has the same number of days (400*365+97), so count the number of complete 400-year periods in x-1
let daysIn400YearEraDayPeriods = (400 * 365 + 97)
let daysIn100YearEraDayPeriods = (100 * 365 + 24)
let daysIn4YearEraDayperiods = (4 * 365 + 1)

let rec countYearPeriods400 eraDay counter =
    match eraDay with
    | 0 -> counter
    | eraDay when (eraDay % daysIn400YearEraDayPeriods) = 0 -> countYearPeriods400 (eraDay-1) (counter+1)
    | eraDay when (eraDay % daysIn400YearEraDayPeriods) <> 0 -> countYearPeriods400 (eraDay-1) counter
    | _ -> 0
printfn "This should be 0: %A" (countYearPeriods400 1 0)
printfn "This should be 1: %A" (countYearPeriods400 146097 0)
printfn "This should be 2: %A" (countYearPeriods400 292194 0)

let leftOverDaysAfterPeriod400 eraDay =
    eraDay - ((countYearPeriods400 eraDay 0) * daysIn400YearEraDayPeriods)
printfn "This should be 1: %A" (leftOverDaysAfterPeriod400 1)
printfn "This should be 0: %A" (leftOverDaysAfterPeriod400 146097)
printfn "This should be 1: %A" (leftOverDaysAfterPeriod400 146098)
printfn "This should be 0: %A" (leftOverDaysAfterPeriod400 292194)

// [Special case: if this yields 4 periods then there's actually only 3.] 
let rec countYearPeriods100 eraDay counter =
    match eraDay with
    | 0 -> if counter = 4 then 3 else counter
    | eraDay when (eraDay % daysIn100YearEraDayPeriods) = 0 -> countYearPeriods100 (eraDay-1) (counter+1)
    | eraDay when (eraDay % daysIn100YearEraDayPeriods) <> 0 -> countYearPeriods100 (eraDay-1) counter
    | _ -> 0
printfn "This should be 0: %A" (countYearPeriods100 1 0)
printfn "This should be 0: %A" (countYearPeriods100 36523 0)
printfn "This should be 2: %A" (countYearPeriods100 73049 0)

let leftOverDaysAfterPeriod100 eraDay =
    eraDay - ((countYearPeriods100 eraDay 0) * daysIn100YearEraDayPeriods)
printfn "This should be 1: %A" (leftOverDaysAfterPeriod100 1)
printfn "This should be 36523: %A" (leftOverDaysAfterPeriod100 36523)
printfn "This should be 1: %A" (leftOverDaysAfterPeriod100 73049)

let rec countYearPeriods4 eraDay counter =
    match eraDay with
    | 0 -> counter
    | eraDay when (eraDay % daysIn4YearEraDayperiods) = 0 -> countYearPeriods4 (eraDay-1) (counter+1)
    | eraDay when (eraDay % daysIn4YearEraDayperiods) <> 0 -> countYearPeriods4 (eraDay-1) counter
    | _ -> 0

let leftOverDaysAfterPeriod4 eraDay =
    eraDay - ((countYearPeriods4 eraDay 0) * daysIn4YearEraDayperiods)

// [Special case: if this yields 4 periods then there's actually only 3.] 
let rec countYears eraDay counter =
    match eraDay with
    | 0 -> if counter = 4 then 3 else counter
    | eraDay when (eraDay % 365) = 0 -> countYears (eraDay-1) (counter+1)
    | eraDay when (eraDay % 365) <> 0 -> countYears (eraDay-1) counter
    | _ -> 0

let leftOverDaysAfterYearPeriod1 eraDay =
    eraDay - ((countYears eraDay 0) * 365)
printfn "This should be 1: %A" (countYears 365 0)
printfn "This should be 3: %A" (countYears 1460 0)
printfn "This should be 3: %A" (countYears 1095 0)

let findYearFromEraDay eraDay =
    let years400 = (countYearPeriods400 eraDay 0) * 400
    let years100 = (countYearPeriods100 (leftOverDaysAfterPeriod400 eraDay) 0) * 100
    let years4 = (countYearPeriods4 (leftOverDaysAfterPeriod100 (leftOverDaysAfterPeriod400 eraDay)) 0) * 4
    let years = countYears (leftOverDaysAfterPeriod4 (leftOverDaysAfterPeriod100 (leftOverDaysAfterPeriod400 eraDay))) 0
    let days = leftOverDaysAfterYearPeriod1 (leftOverDaysAfterPeriod4 (leftOverDaysAfterPeriod100 (leftOverDaysAfterPeriod400 eraDay)))
    let totalYears = years + years4 + years100 + years400
    match days with 
    | 0 -> totalYears
    | days when days > 0 && totalYears = 0 -> 1
    | days when days > 0 && totalYears > 0 -> 1 + totalYears
    | _ -> 0
printfn "This should be 1: %A" (findYearFromEraDay 1)
printfn "This should be 4: %A" (findYearFromEraDay 1461)
printfn "This should be 5: %A" (findYearFromEraDay 1462)
printfn "This should be 622: %A" (findYearFromEraDay 227015)
printfn "This should be 1792: %A" (findYearFromEraDay 654415)
printfn "This should be 1999: %A" (findYearFromEraDay 729825)