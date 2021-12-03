// Learn more about F# at http://fsharp.org

open System
open System.IO

let values = File.ReadLines(@"day01/input.txt")
               |> Seq.map int
               |> Seq.toList

let pairs xs =
    xs
    |> Seq.windowed 2
    |> Seq.map (fun xs -> (xs.[0], xs.[1]))

let noOfAscending (xs: (int * int) seq) =
    xs
    |> Seq.where (fun (x, y) -> y > x)
    |> Seq.length

let answer1 =
    values
    |> pairs
    |> noOfAscending

let windows =
    values
    |> Seq.windowed 3

let windowSums =
    windows
    |> Seq.map Array.sum

let answer2 =
    windowSums
    |> pairs
    |> noOfAscending

printfn $"answer part 1: %A{answer1}"
printfn $"answer part 2: %A{answer2}"