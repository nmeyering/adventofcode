open System.IO
open System
open System.Numerics

let input = File.ReadAllLines ("day13/input.txt") |> Seq.toList
//let input = [
//    "939"
//    "7,13,x,x,59,x,31,19"
//]

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

let lcm x y =
    x * y / (gcd x y)

for (i, n) in allBuses do
    printf "x mod %A = %A, " n i

let modInv (m: bigint) (a: bigint) =
    let rec eea (t: bigint) (t': bigint) (r: bigint) (r': bigint) =
        if r' = 0I then t else
        let div = r/r'
        eea t' (t - div * t') r' (r - div * r')
    (m + eea 0I 1I m a) % m

let bs = allBuses |> List.map snd
let n = bs |> List.reduce lcm
let remainders = allBuses |> List.map fst
let nhats = allBuses |> List.map (snd >> fun ni -> n / ni)
let vs = nhats |> List.map (modInv n)
let es = Seq.zip nhats vs |> Seq.map (fun (v, n) -> v * n)
let x = Seq.zip remainders es |> Seq.sumBy (uncurry (*))
x % n