open System
open System.IO
open System.Text.RegularExpressions

let inputs =
    File.ReadLines(@"day04/input.txt")
    |> Seq.toList

// let inputs = [
//     "2-4,6-8"
//     "2-3,4-5"
//     "5-7,7-9"
//     "2-8,3-7"
//     "6-6,4-6"
//     "2-6,4-8"
// ]

type Range = int * int
let Range x y: Range = x, y

let parsedInputs =
    inputs
    |> List.map (
        fun line -> Regex.Match (line, @"(\d+)-(\d+),(\d+)-(\d+)")
        >> fun m -> m.Groups
        >> fun gs -> (Range (int gs[1].Value) (int gs[2].Value), Range (int gs[3].Value) (int gs[4].Value))
        >> fun ((a, b), (c, d)) -> set [a..b], set [c..d]
    )

let oneContainedByOther a b =
    Set.isSubset a b || Set.isSubset b a

let answer1 =
    parsedInputs
    |> List.filter (fun (a, b) -> oneContainedByOther a b)
    |> List.length

printfn "%A" answer1

let overlaps a b =
    Set.intersect a b |> (not << Set.isEmpty)

let answer2 =
    parsedInputs
    |> List.filter (fun (a, b) -> overlaps a b)
    |> List.length

printfn "%A" answer2