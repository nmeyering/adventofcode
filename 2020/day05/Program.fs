open System
open System.IO

let pickHalf  (q: bool) (low, high) =
    let middle = (low + high) / 2
    if q then
        low, middle
    else   
        middle + 1, high

let seatId row col =
    row * 8 + col

let decode' picker runLength xs =
    xs |> Seq.take runLength
    |> Seq.map picker
    |> Seq.map pickHalf
    |> Seq.fold (fun x f -> f x) (0, pown 2 runLength - 1)
    |> fst

let decode (input: string): (int * int) =
    let row =
        input |> (decode' ((=)'F') 7)
    let col =
        input |> Seq.skip 7 |> (decode' ((=)'L') 3)

    (row, col)

let uncurry f (x, y) = f x y

let lines = File.ReadAllLines(@"day05/input.txt")

let answer1 =
    lines 
    |> Seq.map (decode >> uncurry seatId)
    |> Seq.max

let occupiedSeats =
    lines
    |> Seq.map (decode >> uncurry seatId)
    |> set

let allPossibleSeats w h =
    Seq.allPairs [0..w-1] [0..h-1]
    |> Seq.map (uncurry seatId)
    |> set

Set.count (allPossibleSeats 128 8)

let answer2 =
    Set.difference (allPossibleSeats 128 8) occupiedSeats 
    |> Seq.windowed 3
    |> Seq.tryFind (function [|x; y; z|] -> ((x + 1) <> y && (y + 1) <> z) | _ -> false)
    |> (function | Some [|_; x; _|] -> Some x | _ -> None)