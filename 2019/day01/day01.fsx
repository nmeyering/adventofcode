open System.IO

let fuelCost x =
    x / 3 - 2


let recursiveCosts weight =
    let subCosts fuel =
        if fuel <= 0 then
            None
        else
            Some (fuel, fuelCost fuel)
    fuelCost weight |> Seq.unfold subCosts

let realFuelCost = Seq.sum << recursiveCosts

let weights = File.ReadAllLines(@"day01/input.txt") |> Array.toList |> List.map int

let answer1 = List.sumBy fuelCost weights
let answer2 = List.sumBy realFuelCost weights
