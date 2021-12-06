open System
open System.IO
open System.Numerics

let fish =
    File.ReadLines(@"day06/input.txt")
    |> Seq.head
    |> fun s -> s.Split(',')
    |> Seq.map int
    |> Seq.toList

let evolve (swarm: int list) : int list =
    let evolve' (fish: int) : int list =
        match fish with
        | 0 -> [ 6; 8 ]
        | n -> [ n - 1 ]

    swarm |> List.map evolve' |> List.concat

let rec repeat n f x =
    if n = 0 then
        x
    else
        repeat (n - 1) f (f x)

repeat 80 evolve fish
|> List.length
|> printfn "part 1: %A"

type Population = BigInteger array

let counts = fish |> List.countBy id |> Map

let population =
    [0..8]
    |> List.map (fun i -> Map.tryFind i counts)
    |> List.map (function Some x -> x | None -> 0)
    |> List.map BigInteger
    |> List.toArray

let evolveSmart (population: Population) : Population =
    let dying = population.[0]
    let shifted = population |> Array.skip 1 |> fun x -> Array.append x [| BigInteger 0 |]
    shifted
    |> Array.mapi (fun i x -> match i with
                              | 8 -> dying
                              | 6 -> x + dying
                              | _ -> x)

repeat 256 evolveSmart population
|> Array.sum
|> printfn "part 2: %A"