open System
open System.IO

let inputs = File.ReadLines(@"day09/input.txt") |> Seq.toList

// let inputs = [ "R 4"; "U 4"; "L 3"; "D 1"; "R 4"; "D 1"; "L 5"; "R 2" ]

type Move =
    | R of int
    | U of int
    | L of int
    | D of int

let parseMove (line: string) : Move =
    let mutable intvalue = 0
    let [| a; b |] = line.Split()
    let steps = int b

    match a with
    | "R" -> R steps
    | "D" -> D steps
    | "U" -> U steps
    | "L" -> L steps
    | _ -> failwith "invalid move"

type Pos = int * int

let go (acc: Pos list) (move: Move) : Pos list =
    match acc with
    | [] -> failwith "no starting pos"
    | (x, y) :: _ ->
        let newSteps =
            match move with
            | U n -> [ 1..n ] |> List.map (fun o -> (x, y - o))
            | D n -> [ 1..n ] |> List.map (fun o -> (x, y + o))
            | R n -> [ 1..n ] |> List.map (fun o -> (x + o, y))
            | L n -> [ 1..n ] |> List.map (fun o -> (x - o, y))

        List.append (newSteps |> List.rev) acc


let steps = inputs |> List.map parseMove |> List.fold go [ (0, 0) ] |> List.rev

let follow' (trail: Pos list) (headPos: Pos) : Pos list =
    match trail with
    | [] -> [ headPos ]
    | pos :: _ ->
        match pos, headPos with
        | (x, y), (hx, hy) when abs (hx - x) <= 1 && abs (hy - y) <= 1 -> trail
        | (x, y), (hx, hy) ->
            let dx = sign (hx - x)
            let dy = sign (hy - y)
            (x + dx, y + dy) :: trail

let follow (steps: Pos list) : Pos list =
    steps |> List.fold follow' [] |> List.rev

let count steps = steps |> List.distinct |> List.length

steps |> follow |> count |> printfn "Part 1: %A"

let rec iterate n f x =
    if n = 0 then x else iterate (n - 1) f (f x)

steps |> iterate 9 follow |> count |> printfn "Part 2: %A"
