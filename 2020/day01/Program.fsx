// Learn more about F# at http://fsharp.org

open System
open System.IO

let rec pairs lst =
    match lst with
    | [] -> []
    | h::t -> List.map (fun x -> (h, x)) t @ pairs t

let rec triples lst =
    match lst with
    | [] -> []
    | h::t -> List.map (fun (x, y) -> (h, x, y)) (pairs t) @ triples t

let expenses = File.ReadLines(@"day01/input.txt") |> Seq.map int |> Seq.toList

pairs expenses
|> List.filter (fun (x, y) -> x + y = 2020)
|> List.tryHead
|> Option.map (fun (x, y) -> x * y)

triples expenses
|> List.filter (fun (x, y, z) -> x + y + z = 2020)
|> List.tryHead
|> Option.map (fun (x, y, z) -> x * y * z)

