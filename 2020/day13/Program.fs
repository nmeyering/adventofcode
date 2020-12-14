open System.IO
open System

// let input = File.ReadAllLines ("day13/input.txt") |> Seq.toList
let input = [
    "939"
    "7,13,x,x,59,x,31,19"
]

let startTime = input |> List.head |> int
let buses =
    input
    |> List.skip 1
    |> List.head
    |> fun s -> s.Split ',' |> Array.toList
    |> List.filter (fun s -> s <> "x")
    |> List.map int

let uncurry f (x, y) = f x y

let answer1 =
    buses
    |> List.map (fun b -> (b, b - startTime % b))
    |> List.minBy snd
    |> uncurry (*)

let allBuses =
    input |> List.skip 1 |> List.head
    |> fun s -> s.Split ',' |> Array.toList
    |> List.map (Int32.TryParse >> snd)
    |> List.mapi (fun i x -> (bigint i, bigint x))
    |> List.filter (fun (i, x) -> x <> 0I)

let rec gcd (x: bigint) (y: bigint) =
    if x = 0I then y else
    if y = 0I then x else
    if x > y then
        gcd y (x % y)
    else
        gcd x (y % x)


// let gcd2 x y =
//     let d = gcd x y
//     x / d, y / d

// let lcm x y =
//     x * y / (gcd x y)

let aligned (buses: (bigint * bigint) list) (n: bigint): bool =
    buses
    |> List.fold (fun state (i, x) -> if state then x - n % x = i else false) true

let rec f (n: bigint) =
    match n with 
    | n when aligned allBuses n -> n
    | _ -> f (n + 7I)

f 0I