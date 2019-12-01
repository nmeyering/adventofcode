// Learn more about F# at http://fsharp.org

open System
open System.IO

let fuelCost x =
    x / 3 - 2

let lines = File.ReadAllLines(@"day01/input.txt") |> Array.toList |> List.map int

List.sumBy fuelCost lines
