open System.IO

let uncurry f (x, y) = f x y
let flip f x y = f y x
let prepend x xs = x :: xs

let lines = File.ReadAllLines ("day10/input.txt") |> Seq.map int |> Seq.toList

let answer1 =
 lines
 |> List.sort
 |> prepend 0
 |> List.pairwise
 |> List.map ((-) |> flip |> uncurry)
 |> List.countBy id
 |> List.map (fun (x, y) -> if x = 3 then 1 + y else y)
 |> List.reduce (*)

let rec foo (xs: (int * bigint) list): bigint =
    match xs with
    | [] -> bigint 0
    | [(x, paths)] -> paths
    | (x, paths) :: t ->
        let fits (n, _) = (n - x) < 4
        let succ = t |> List.takeWhile fits
        let rest = t |> List.skipWhile fits
        let succ' = succ |> List.map (fun (n, p) -> (n, p + paths))
        foo (succ' @ rest)

let answer2 = 
    lines
    |> List.sort
    |> List.map (fun x -> (x, bigint 0))
    |> fun xs -> (0, bigint 1) :: xs
    |> foo
