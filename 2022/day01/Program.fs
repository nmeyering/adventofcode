module Day01

open System
open System.IO
open Advent

let elves =
    File.ReadLines(@"day01/input.txt")
    |> splitOn (fun x -> x = "")
    
let calories = elves |> Seq.map (List.sumBy int)
let answer1 = calories |> Seq.max

printfn "Part 1: %A" answer1

let answer2 = calories |> Seq.toList |> List.sortDescending |> List.take 3  |> List.sum

printfn "Part 2: %A" answer2