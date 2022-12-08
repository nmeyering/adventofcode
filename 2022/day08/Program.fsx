open System
open System.IO
open System.Text.RegularExpressions

let inputs = File.ReadLines(@"day08/input.txt") |> Seq.toArray

// let inputs = [|
//     "30373"
//     "25512"
//     "65332"
//     "33549"
//     "35390"
// |]

let forest =
    inputs
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let lookFromLeft =
    let f =
        Array.scan (fun (_, max) y -> if y > max then (true, y) else (false, max)) (true, -1)

    Array.map (fun line -> line |> f |> Array.skip 1)

let lookFromRight = Array.map (Array.rev) >> lookFromLeft >> Array.map (Array.rev)
let lookFromTop = Array.transpose >> lookFromLeft >> Array.transpose
let lookFromBottom = Array.transpose >> lookFromRight >> Array.transpose

forest
|> (fun f ->
    let a = lookFromLeft f |> array2D
    let b = lookFromRight f |> array2D
    let c = lookFromTop f |> array2D
    let d = lookFromBottom f |> array2D
    let mutable sum = 0

    a
    |> Array2D.mapi (fun x y (visible, _) -> visible || (fst b.[x, y]) || (fst c.[x, y]) || (fst d.[x, y]))
    |> Array2D.iter (fun x -> if x then sum <- sum + 1 else ())

    sum)
|> printfn "Part 1: %A"

module Array2D =
    let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
        let mutable state = state

        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                state <- folder x y state (array.[x, y])

        state

let scenicScore (arr: int array array) (x: int) (y: int) tree =
    let len = Array.length arr
    let flipped = arr |> Array.transpose

    let directions =
        List.map
            (fun x -> Array.skip 1 x)
            [ arr.[x][y..len]
              arr.[x][0..y] |> Array.rev
              flipped.[y][x..len]
              flipped.[y][0..x] |> Array.rev ]

    let view (trees: int array) =
        if Array.isEmpty trees then
            0
        else
            trees
            |> Array.takeWhile (fun x -> x < tree)
            |> Array.length
            |> fun x -> min (Array.length trees) (x + 1)

    let xs = directions |> List.map view
    xs |> List.reduce (fun x y -> x * y)

forest
|> array2D
|> Array2D.mapi (scenicScore forest)
|> Array2D.foldi (fun _ _ best score -> max best score) 0
|> printfn "Part 2: %A"
