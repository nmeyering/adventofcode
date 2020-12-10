open System.IO

let input = File.ReadAllLines ("day09/input.txt") |> Seq.map int64 |> Seq.toList

let allPairs' xs = List.allPairs xs xs

let uncurry f (x, y) = f x y

let isValid (xs: int64 list) =
    match List.rev xs with
    | [] -> false
    | x :: rest ->
        rest 
            |> allPairs'
            |> List.map (uncurry (+))
            |> (not << List.contains x)

let answer1 = 
    input
    |> List.windowed 26
    |> List.tryFind isValid 
    |> Option.map List.last

let sum': (int64 list -> int64) =
    let f (x: int64) (y: int64): int64 =
        x + y
    List.reduce f

let addSmallestAndLargest (xs: int64 list) =
    let min = List.min xs
    let max = List.max xs
    min + max

let answer2 =
    answer1
    |> Option.bind (fun answer1' ->
        [2..List.length input]
        |> List.collect (fun x -> (List.windowed x input))
        |> List.tryFind (sum' >> (=)answer1')
        |> Option.map addSmallestAndLargest
    )